module Network.SSH.Client.Hssh.Core where

import qualified Data.ByteString as S
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Reader (ask, asks)
import Control.Applicative ((<*>))
import Data.Functor ((<$>))
import System.IO (stderr, hPutStrLn)
import Data.Conduit
import Data.Conduit.Cereal (conduitGet, sinkGet, conduitPut)
import Data.Serialize.Put (runPut)
import Data.Serialize.Get (runGet)

import Data.Conduit.Network ( Application, clientSettings, runTCPClient
                            , appSource, appSink )

import Network.SSH.Client.Hssh.Monad (Ssh, SshSettings(..), runSsh)
import Network.SSH.Client.Hssh.Messages
import Network.SSH.Client.Hssh.Packet

debug :: (MonadIO m) => String -> m ()
debug = liftIO . (hPutStrLn stderr)

logic :: Conduit Packet Ssh Packet
logic = do
    handshake
    Just (hs @(Handshake sw)) <- await
    debug $ show hs
    go
  where
    go = do
        mbInput <- await
        case mbInput of
          Just p@Packet {..} ->
              do
                debug $ show p
                _msg <- parseSshMsg packetPayload
                sendSshMsg $ KexInit [["diffie-hellman-group1-sha1"], ["ssh-dss", "ssh-rsa"], ["aes256-cbc"], ["aes256-cbc"], ["hmac-sha1"], ["hmac-sha1"], ["none"], ["none"], [], []] False
--                yield p
                go
          Nothing -> return ()
    sendSshMsg msg = do
        debug $ "  MSG OUT: " ++ show msg
        let p = Packet { packetPayload = runPut $ serialize msg
                       , packetMac = Nothing }
        debug $ " OUT: " ++ show p
        yield p
    parseSshMsg raw = do
        let msg = runGet parse raw
        debug $ "  MSG IN: " ++ show msg
        return msg
    handshake = asks sshsSoftware >>= yield . Handshake

runClient :: SshSettings -> IO ()
runClient s@SshSettings{..} = runSsh s $ do
    runTCPClient (clientSettings sshsPort sshsHost) app
  where
    app d = (appSource d) $= pipeline $$ (appSink d)

pipeline :: Conduit S.ByteString Ssh S.ByteString
pipeline = parser =$= logic =$= serializer
  where
    parser = do
      sinkGet handshake >>= yield
      conduitGet $ parsePacket False
    serializer = do
      conduitPut serializePacket
