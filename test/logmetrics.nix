{ config, pkgs, lib, ... }:

with pkgs;

let logmetrics = pkgs.haskellPackages.callPackage ./.. {}; in

{

  config = {
    systemd.sockets.logmetrics = {
      wantedBy = [ "sockets.target" ];
      socketConfig = {
        ListenStream = "0.0.0.0:9100";
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
            "metricsMaxBodySize": 250,
            "metricsMaxChunkSize": 100,
            "metrics": [
              {
                "name": "foo.bar.scsi.count",
                "matchField": {
                  "match": "contains",
                  "field": "kernel_device",
                  "value": "scsi"
                },
                "setTags": {"type": "hw"},
                "mapTags": {"hostname": "host", "missing": "foobarqux"}
              },
              {
                "name": "problem.count",
                "matchAny": [
                  {"matchField": {"match": "contains", "field": "message", "value": "error"}},
                  {"matchField": {"match": "contains", "field": "message", "value": "Error"}},
                  {"matchField": {"match": "contains", "field": "message", "value": "warning"}},
                  {"matchField": {"match": "contains", "field": "message", "value": "Warning"}}
                ],
                "setTags": {"environment": "test"},
                "mapTags": {"service": "systemd_unit"},
                "inheritTags": ["pid"]
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
