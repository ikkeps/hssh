module Network.SSH.Client.Hssh.Core (runClient) where

import qualified Data.ByteString as S
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Reader (ask, asks)
import Control.Monad.State (get, gets, modify)
import System.IO (stderr, hPutStrLn)
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Conduit.Cereal (conduitGet, sinkGet, conduitPut, sourcePut)
import Data.Serialize.Put (runPut)
import Data.Serialize.Get (runGet)

import Data.Conduit.Network ( Application, clientSettings, runTCPClient
                            , appSource, appSink )

import Network.SSH.Client.Hssh.Prelude
import Network.SSH.Client.Hssh.Monad (Ssh, SshSettings(..), SshState(..), runSsh)
import Network.SSH.Client.Hssh.Messages
import Network.SSH.Client.Hssh.Packet


runClient :: SshSettings -> IO ()
runClient s@SshSettings{..} = runSsh s $ do
    runTCPClient (clientSettings sshsPort sshsHost) app
  where
    app d = (appSource d) $= pipeline $$ (appSink d)

logic :: Conduit SshMessage Ssh SshMessage
logic = go
  where
    go = do
        mbInput <- await
        case mbInput of
          Just msg ->
              do
                debug " Got message:"
                debug $ show msg
                yield $ KexInit [["diffie-hellman-group1-sha1"], ["ssh-dss", "ssh-rsa"], ["aes256-cbc"], ["aes256-cbc"], ["hmac-sha1"], ["hmac-sha1"], ["none"], ["none"], [], []] False
                go
          Nothing -> return ()

pipeline :: Conduit S.ByteString Ssh S.ByteString
pipeline = parser =$= packetPipeline =$= serializer
  where
    parser = do
      sw <- sinkGet parseHandshake
      debug $ show sw
      forever $ do
        seqNum <- gets sshstInputSeqNumber
        debug $ "IN #" ++ (show seqNum)
        -- FIXME: using sinkGet here is bad, need to find better way
        lift packetParser >>= sinkGet >>= yield
        bumpInSeqNumber
    serializer = do
        ask >>= yield . runPut . serializeHandshake
        CL.mapM $ \packet -> do
            seqNum <- gets sshstOutputSeqNumber
            debug $ "OUT #" ++ (show seqNum)
            -- FIXME: Using bare runPut seems unefficient to me
            bin <- runPut <$> packetSerializer packet
            bumpOutSeqNumber
            return bin
    -- Lens, anyone?
    bumpInSeqNumber =
      modify (\s@SshState{sshstInputSeqNumber} ->
               s{sshstInputSeqNumber=sshstInputSeqNumber+1})
    bumpOutSeqNumber =
      modify (\s@SshState{sshstOutputSeqNumber} ->
               s{sshstOutputSeqNumber=sshstOutputSeqNumber+1})

packetPipeline :: Conduit Packet Ssh Packet
packetPipeline = extractMessage =$= logic =$= wrapMessage
  where
    extractMessage = CL.map packetPayload =$= conduitGet parse
    wrapMessage = conduitPut serialize =$= CL.map Packet

debug :: (MonadIO m) => String -> m ()
debug = liftIO . (hPutStrLn stderr)
