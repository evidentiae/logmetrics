{ config, pkgs, lib, ... }:

with pkgs;

let logmetrics = pkgs.haskellPackages.callPackage ./.. {}; in

{

  config = {
    systemd.services.logmetrics = {
      wantedBy = [ "multi-user.target" ];
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
        ExecStart = "${logmetrics}/bin/logmetrics";
        ListenPort = 9100;
        Restart = "always";
      };
    };
  };

}
