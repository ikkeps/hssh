module Network.SSH.Client.Hssh.Core where

import Data.Word (Word32, Word8)
import Data.ByteString.Lazy (ByteString, snoc, empty, fromStrict)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as S
import Data.Serialize.Get ( Get, getWord32be, getWord8
                          , getByteString, skip, runGet, runGetLazy
                          , getLazyByteString )
import Data.Serialize.Put ( Put, runPut )
import Control.Monad.IO.Class (liftIO, MonadIO)
import System.IO ( stdin, stdout, stderr
                 , hPutStrLn, hSetBuffering, BufferMode(NoBuffering))
import Data.Conduit
import Data.Conduit.Util (sequenceSink, SequencedSinkResponse(..))
import Data.Conduit.Binary as CB
import Data.Conduit.Cereal (conduitGet, sinkGet, conduitPut)

import Network.SSH.Client.Hssh.Messages


data Packet = Handshake { handshakeSoftware :: ByteString }
            | Packet ByteString -- already decoded and unpacked data
                deriving (Show)



debug :: (MonadIO m) => String -> m ()
debug = liftIO . (hPutStrLn stderr)

parseSshMsg :: Get SshMessage
parseSshMsg = do
    code <- getWord8
    case code of
        1  -> (parseDisconnect)-- SSH_MSG_DISONNECT
        2  -> (parseIgnore)    -- SSH_MSG_IGNORE
        20 -> (parseKexInit)   -- SSH_MSG_KEX_INIT
        _  -> fail $ "unknown code: " ++ show code

serializeSshMsg :: SshMessage -> Put
serializeSshMsg = serialize


core :: (MonadIO m) => Conduit Packet m Packet
core = do
    yield $ Handshake $ BL.fromStrict $ S.pack "hssh"
    go
  where
    go = do
        mbInput <- await
        debug $ "  << " ++ show mbInput
        case mbInput of
          Just (Handshake _) -> go
          Just packet ->
              do
                debug $ "  >> " ++ show packet
                sendSshMsg $ Ignore $ BL.pack [1..123]
                go
          Nothing -> return ()
    sendSshMsg msg = do
        yield $ Packet $ BL.fromStrict $ runPut $ serializeSshMsg msg
