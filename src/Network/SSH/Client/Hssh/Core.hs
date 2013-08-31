module Network.SSH.Client.Hssh.Core where

import qualified Data.ByteString as S
import Control.Monad.IO.Class (liftIO, MonadIO)
import System.IO (stderr, hPutStrLn)
import Data.Conduit
import Data.Conduit.Cereal (conduitGet, sinkGet, conduitPut)
import Data.Serialize.Put (runPut)
import Data.Serialize.Get (runGet)

import Network.SSH.Client.Hssh.Messages
import Network.SSH.Client.Hssh.Packet

debug :: (MonadIO m) => String -> m ()
debug = liftIO . (hPutStrLn stderr)

logic :: (MonadIO m) => Conduit Packet m Packet
logic = do
    yield handshake
    debug $ show handshake
    go
  where
    go = do
        mbInput <- await
        case mbInput of
          Just hs@(Handshake _) -> do
              debug $ show hs
              go
          Just Packet {..} ->
              do
                _msg <- parseSshMsg packetPayload
                sendSshMsg $ Ignore $ S.pack [1..123]
                go
          Nothing -> return ()
    sendSshMsg msg = do
        debug $ "  MSG OUT: " ++ show msg
        yield $ Packet { packetPayload = runPut $ serialize msg
                       , packetMac = Nothing }
    parseSshMsg raw = do
        let msg = runGet parse raw
        debug $ "  MSG IN: " ++ show msg
        return msg
    handshake = Handshake "hssh"

pipeline :: (MonadIO m, MonadThrow m) => Conduit S.ByteString m S.ByteString
pipeline = parser =$= logic =$= serializer
  where
    parser = do
      sinkGet handshake >>= yield
      conduitGet $ parsePacket False
    serializer = do
      conduitPut serializePacket
