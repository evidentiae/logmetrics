{ config, pkgs, lib, ... }:

with lib;

let

  elasticdump = pkgs.callPackage ./pkgs/elasticdump {};

  opentsdb-logback = pkgs.writeText "logback.xml" ''
    <?xml version="1.0" encoding="UTF-8"?>
    <configuration>
      <appender name="STDOUT" class="ch.qos.logback.core.ConsoleAppender">
        <encoder>
          <pattern>
            %-5level [%thread] %logger{0}: %msg%n
          </pattern>
        </encoder>
      </appender>
      <root level="WARN">
        <appender-ref ref="STDOUT"/>
      </root>
    </configuration>
  '';

in {
  imports = [
    <imsl-nix-modules/testing/libvirt.nix>
  ];

  libvirt.test.instances.logserver = {
    libvirt.memory = 2048;
    libvirt.cpuCount = 2;
    nixos.modules = singleton {
      imports = [
        ./monitored.nix
        ./logmetrics.nix
      ];
      services.elasticsearch = {
        enable = true;
        dataDir = "/var/elasticsearch";
        extraConf = ''
          network:
            bind_host: 0.0.0.0
          node:
            name: logserver
            master: true
            data: true
        '';
      };
      services.bosun = {
        enable = true;
      };
      services.opentsdb = {
        enable = true;
      };
      systemd.services.opentsdb.environment.JVMARGS =
        "-Dlogback.configurationFile=file://${opentsdb-logback}";
      services.hbase = {
        enable = true;
      };
      systemd.services.hbase.environment.HBASE_ROOT_LOGGER =
        "WARN,console";
    };
  };

  libvirt.test.test-driver.scriptPath = with pkgs; [
    fping
    curl
    netcat
    elasticdump
    nmap
    sshpass
    openssh
    jq
  ];

  libvirt.test.test-driver.script = pkgs.writeScript "simple-test" ''
    #!${pkgs.bash}/bin/bash

    function query_metrics() {
      local query="$(echo -n "$1" | jq -s -R -r @uri)"
      curl -s "http://logserver:4242/api/query?start=1d-ago&m=$query"
    }

    function list_metrics() {
      curl -s "http://logserver:4242/api/suggest?type=metrics&max=4096"
    }

    function dump_logs() {
      elasticdump \
        --all=true \
        --input=http://logserver:9200/ \
        --output=logs.json

      list_metrics > metrics-names.json

      #query_metrics > metrics.json \
      #  'sum:nginx.request.count{path=*,host=*,servername=*}'
    }

    trap dump_logs SIGINT SIGTERM EXIT

    # Wait for monitor server to start
    while ! nc -z logserver 9200; do echo "waiting for logserver:9200 (elasticsearch)"; sleep 2; done
    while ! nc -z logserver 9100; do echo "waiting for logserver:9100 (logmetrics)"; sleep 2; done
    while ! nc -z logserver 4242; do echo "waiting for logserver:4242 (hbase)"; sleep 2; done
    while ! nc -z logserver 8070; do echo "waiting for logserver:8070 (bosun)"; sleep 2; done

    # Give logs and metrics some time to flush
    sleep 30

    list_metrics 
  '';

}
