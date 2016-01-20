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
            "metrics": [{
              "name": "scsi",
              "matches": [{
                "match": "contains",
                "field": "kernel_device",
                "value": "scsi"
              }],
              "tags": {"type": "hw"},
              "tagsFromFields": {"hostname": "host"}
            }]
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
