module Network.SSH.Client.Hssh.Core where

import qualified Data.ByteString.Char8 as S
import Control.Monad.IO.Class (liftIO, MonadIO)
import System.IO (stderr, hPutStrLn)
import Data.Conduit
import Data.Conduit.Cereal (conduitGet, sinkGet, conduitPut)
import Data.Serialize (runPut, runGet)
import qualified Data.ByteString.Lazy as BL

import Network.SSH.Client.Hssh.Messages
import Network.SSH.Client.Hssh.Packet

debug :: (MonadIO m) => String -> m ()
debug = liftIO . (hPutStrLn stderr)

logic :: (MonadIO m) => Conduit Packet m Packet
logic = do
    yield $ Handshake $ BL.fromStrict $ S.pack "hssh"
    go
  where
    go = do
        mbInput <- await
        debug $ "  << " ++ show mbInput
        case mbInput of
          Just (Handshake _) -> go
          Just (Packet packet) ->
              do
                msg <- parseSshMsg packet
                sendSshMsg $ Ignore $ BL.pack [1..123]
                go
          Nothing -> return ()
    sendSshMsg msg = do
        debug $ "  MSG OUT: " ++ show msg
        yield $ Packet $ BL.fromStrict $ runPut $ serialize msg
    parseSshMsg raw = do
        let msg = runGet parse raw
        debug $ "  MSG IN: " ++ show msg
        return msg

pipeline :: (MonadIO m, MonadThrow m) => Conduit S.ByteString m S.ByteString
pipeline = parser =$= logic =$= serializer
  where
    parser = do
      sinkGet handshake >>= yield
      conduitGet $ parsePacket False
    serializer = do
      conduitPut serializePacket
