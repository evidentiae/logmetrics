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

  # A simple daemon that tails journald events, does some simple processing (see
  # the fieldMap function), formats the events as JSON and then sends them to
  # fluentd
  journal-fluentd = writeHaskellScript {
    name = "journal-fluentd";
    pkgfun = p: [
      p.aeson p.pipes-aeson p.pipes p.pipes-safe p.pipes-bytestring
      p.pipes-network p.iso8601-time p.time p.network
      p.libsystemd-journal
    ];
  } ''
    {-# LANGUAGE OverloadedStrings #-}
    {-# LANGUAGE PartialTypeSignatures #-}
    {-# LANGUAGE ViewPatterns #-}
    import Systemd.Journal
    import Pipes
    import Pipes.Safe
    import Pipes.Aeson
    import Pipes.Network.TCP ( toSocket )
    import Network.Socket
    import qualified Pipes.Prelude as P
    import qualified System.Posix.Files
    import qualified Data.ByteString as BS
    import qualified Data.HashMap.Strict as HM
    import qualified Data.Text as T
    import qualified Data.Aeson as A
    import Data.Text.Encoding
    import Data.Time.Clock.POSIX
    import Data.Time.ISO8601
    import Data.Ratio ( (%) )
    import Data.List ( find )

    cursorFile = "/var/journal-fluentd.state"
    tmpCursorFile = "/var/journal-fluentd.state.tmp"

    main = do
      o <- out
      hasCursor <- System.Posix.Files.fileExist cursorFile
      start <- if hasCursor
               then fmap (flip FromCursor Forwards) (BS.readFile cursorFile)
               else return (FromStart) -- Forwards)
      runSafeT $ runEffect $ src start >-> P.chain checkpoint >-> encode >-> o

    out :: IO (Consumer BS.ByteString (SafeT IO) ())
    out = do
      addrInfo <- getAddrInfo Nothing (Just "127.0.0.1") (Just "2000")
      let serverAddr = head addrInfo
      sock <- socket (addrFamily serverAddr) Stream defaultProtocol
      connect sock (addrAddress serverAddr)
      return $ toSocket sock

    src :: Start -> Producer JournalEntry (SafeT IO) ()
    src start = openJournal [] start Nothing Nothing

    encode :: Pipe JournalEntry BS.ByteString (SafeT IO) ()
    encode = for (P.map tojson) $ \e ->
               for (encodeObject e) $ \bs -> yield bs >> yield "\n"

    tojson :: JournalEntry -> HM.HashMap T.Text A.Value
    tojson je = HM.fromList $ timestamp : cursor : map fieldMap kvs
      where
        safeUtf8 (decodeUtf8' -> Right t) = t
        safeUtf8 _ = "[DATA]"
        kvs = [(journalField k, safeUtf8 v) | (k,v) <- HM.toList (journalEntryFields je)]
        cursor = ("cursor", A.String $ safeUtf8 (journalEntryCursor je))
        timestamp = ("@timestamp"
                    , A.String $ T.pack $ formatISO8601 $
                      posixSecondsToUTCTime $ fromRational $ us % 1000000)
        us = case find ((== "_SOURCE_REALTIME_TIMESTAMP") . fst) kvs of
               Just (_,v) -> read (T.unpack v)
               Nothing -> toInteger $ journalEntryRealtime je

    fieldMap :: (T.Text, T.Text) -> (T.Text, A.Value)
    fieldMap ("MESSAGE", v) = ("message", A.String v)
    fieldMap ("_HOSTNAME", v) = ("host", A.String $ T.takeWhile ((/=) '.') v)
    fieldMap ("SYSLOG_IDENTIFIER", v) = ("ident", A.String v)
    fieldMap (k, v) = (T.toLower . T.dropWhile ((==) '_') $ k, A.String v)

    checkpoint :: JournalEntry -> (SafeT IO) ()
    checkpoint entry = liftIO $ do
      BS.writeFile tmpCursorFile (journalEntryCursor entry)
      System.Posix.Files.rename tmpCursorFile cursorFile
  '';

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

    systemd.services.journal-fluentd = {
      wantedBy = [ "multi-user.target" ];
      after = [ "systemd-journald.service" ];
      serviceConfig = {
        Restart = "always";
        RestartSec = 10;
        ExecStart = "${journal-fluentd}/bin/journal-fluentd";
      };
    };
  };

}
