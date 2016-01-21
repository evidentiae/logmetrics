{ config, pkgs, lib, ... }:

with pkgs;

let logmetrics = pkgs.haskellPackages.callPackage ./.. {}; in

{

  config = {
    systemd.sockets.logmetrics = {
      wantedBy = [ "sockets.target" ];
      socketConfig = {
        ListenStream = "logserver:9100";
        # Setup socket similar to how Warp does it
        Backlog = 2048;
        NoDelay = true;
      };
    };

    systemd.services.logmetrics = {
      environment = {
        LOGMETRICS_CONFIG = pkgs.writeText "logmetrics.json" ''
          {
            "port": 9100,
            "logHost": "logserver",
            "logPort": 9200,
            "metricsHost": "logserver",
            "metricsPort": 8070,
            "metricsInterval": 3000,
            "metrics": [
              {
                "name": "foo.bar.scsi.count",
                "matches": [{
                  "match": "contains",
                  "field": "kernel_device",
                  "value": "scsi"
                }],
                "tags": {"type": "hw"},
                "tagsFromFields": {"hostname": "host", "missing": "foobarqux"}
              },
              {
                "name": "problem.count",
                "matches": [
                  {
                    "match": "contains",
                    "field": "message",
                    "value": "Error"
                  },
                  {
                    "match": "contains",
                    "field": "message",
                    "value": "error"
                  },
                  {
                    "match": "contains",
                    "field": "message",
                    "value": "Warning"
                  },
                  {
                    "match": "contains",
                    "field": "message",
                    "value": "WARN"
                  }
                ],
                "tags": {"environment": "test"},
                "tagsFromFields": {"service": "systemd_unit", "pid": "pid"}
              }
            ]
          }
        '';
      };
      serviceConfig = {
        RestartSec = "10s";
        ListenPort = 9100;
        ExecStart = "${logmetrics}/bin/logmetrics";
        # GHC expects all sockets to be non-blocking. If false (the default),
        # warp will simply not produce any output (if using sockets from systemd)
        # Hinted in http://www.yesodweb.com/blog/2012/10/avoid-syscall
        NonBlocking = true;
      };
    };
  };

}
