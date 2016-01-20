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
            "metricsHost": "todo",
            "metricsPort": 55555,
            "metricsInterval": 3000
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
