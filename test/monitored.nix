{ config, pkgs, lib, ... }:

with pkgs;
with lib;

let

  writeHaskellScript = {
    name, pkgfun ? (_: []), buildInputs ? [], extraModules ? []
  }: script: pkgs.stdenv.mkDerivation {
    inherit name;

    phases = [ "buildPhase" "fixupPhase" ];

    buildInputs = buildInputs ++ [
      (pkgs.haskellPackages.ghcWithPackages pkgfun)
    ];

    allModules = pkgs.lib.imap
      (i: m: pkgs.writeText "${toString i}.hs" m)
      (extraModules ++ [ script ]);

    buildPhase = ''
      mkdir -p $out/bin
      ghc --make -O2 -o "$out/bin/$name" -threaded \
        -Wall -fno-warn-missing-signatures $allModules
    '';
  };

  journal2fluentd = pkgs.haskellPackages.callPackage <journal2fluentd> {};

  fluentdConfig = pkgs.writeText "fluentd.conf" ''
    <source>
      @type tcp
      tag json-tcp
      port 2001
      bind 127.0.0.1
      format json
    </source>

    <match fluent.**>
      @type stdout
    </match>

    <match **>
      @type elasticsearch
      @id elasticsearch
      host logserver
      port 9100
      reload_on_failure true
      logstash_format true
      logstash_prefix logs
      utc_index true
      buffer_type memory
      flush_interval 0s
    </match>
  '';

in {

  config = {
    systemd.sockets.proxy-to-fluentd-json = {
      wantedBy = [ "sockets.target" ];
      socketConfig = {
        ListenStream = "127.0.0.1:2000";
      };
    };

    systemd.services.proxy-to-fluentd-json = {
      requires = [ "fluentd.service" ];
      after = [ "fluentd.service" ];
      serviceConfig.ExecStart = "${systemd}/lib/systemd/systemd-socket-proxyd 127.0.0.1:2001";
    };

    systemd.services.fluentd = {
      path = with pkgs; [ which inetutils iproute ];
      serviceConfig = {
        PIDFile = "/run/fluentd.pid";
        Type = "forking";
        TimeoutStartSec = "5min";
        ExecStart = "${writeScriptBin "fluentd" ''
          #!${bash}/bin/bash
          ${fluentd}/bin/fluentd --daemon /run/fluentd.pid \
            --suppress-config-dump --use-v1-config -c ${fluentdConfig}
        ''}/bin/fluentd";
      };
    };

    systemd.services.journal2fluentd = {
      wantedBy = [ "multi-user.target" ];
      after = [ "systemd-journald.service" ];
      environment = {
        JOURNAL2FLUENTD_CONFIG = pkgs.writeText "journal2fluentd.json" ''
          {
            "cursorFile": "/var/journal2fluentd.state",
            "fluentdHost": "127.0.0.1",
            "fluentdPort": 2000
          }'';
      };
      serviceConfig = {
        Restart = "always";
        RestartSec = 10;
        ExecStart = "${journal2fluentd}/bin/journal2fluentd";
      };
    };
  };

}
