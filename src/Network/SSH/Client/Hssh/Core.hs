module Network.SSH.Client.Hssh.Core (runClient) where

import qualified Data.ByteString as S
import Control.Monad.IO.Class (liftIO, MonadIO)
import qualified Data.List as List
import System.IO (stderr, hPutStrLn)
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Conduit.Cereal (conduitGet, sinkGet, conduitPut)
import Data.Serialize.Put (runPut)

import Data.Conduit.Network ( clientSettings, runTCPClient, appSource, appSink )

import Network.SSH.Client.Hssh.Prelude
import Network.SSH.Client.Hssh.Monad (Ssh, SshSettings(..), SshState(..),
                                      runSsh)
import Network.SSH.Client.Hssh.Cipher (Cipher(..))
import Network.SSH.Client.Hssh.Mac (Mac(..))
import Network.SSH.Client.Hssh.Kex (Kex(..))
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
        debug $ "Sending KEX_INIT..."
        lift (mkKexInit False) >>= yield
        debug $ "Awaiting for KEX_INIT..."
        awaitForever $ \kexInit -> do
          negotiateAll kexInit
          showNegotiated
          keyExchange

    negotiateAll KexInit {..} = do
      SshSettings {..} <- ask
      macOut <- lift $ negotiateMac kexInitMacClientToServer sshsMacClientToServer
      modify $ \s -> s{sshstMacClientToServer = macOut}

      macIn <- lift $ negotiateMac kexInitMacServerToClient sshsMacServerToClient
      modify $ \s -> s{sshstMacServerToClient = macIn}

      cipherOut <- lift $ negotiateCipher kexInitEncriptionClientToServer sshsEncriptionClientToServer
      modify $ \s -> s{sshstEncriptionClientToServer = cipherOut}

      cipherIn <- lift $ negotiateCipher kexInitEncriptionServerToClient sshsEncriptionServerToClient
      modify $ \s -> s{sshstEncriptionServerToClient = cipherIn}
    negotiateAll msg = error $ "Unexpected message: " <> show msg
    keyExchange = yield $ KexDhInit 666

    showNegotiated = do
      SshState {..} <- get
      debug $ "Negotiated " <> (show $ kexName sshstKexAlgorithm)
                     <> " " <> (show $ cipherName sshstEncriptionClientToServer)
                     <> "/" <> (show $ cipherName sshstEncriptionServerToClient)
                     <> " " <> (show $ macName sshstMacClientToServer)
                     <> "/" <> (show $ macName sshstMacServerToClient)
      debug $ ""


negotiateMac :: [S.ByteString] -> [Mac] -> Ssh Mac
negotiateMac = negotiate macName

negotiateCipher :: [S.ByteString] -> [Cipher] -> Ssh Cipher
negotiateCipher = negotiate cipherName

negotiate :: (a -> S.ByteString) -> [S.ByteString] -> [a] -> Ssh a
negotiate name server's client's = do
    case List.find (\a -> List.elem (name a) server's) client's of
      Just a -> return a
      Nothing -> error "Negotiation error"


mkKexInit :: Bool -> Ssh SshMessage
mkKexInit packetFollows = do
    SshSettings {..} <- ask
    return $ KexInit {
          kexInitAlgorithms = ["diffie-hellman-group-exchange-sha256"] -- map kexName sshsKexAlgorithms
        , kexInitServerHostKeyAlgorithms = ["ssh-rsa","ssh-dss","ecdsa-sha2-nistp256"]
        , kexInitEncriptionServerToClient = map cipherName sshsEncriptionServerToClient
        , kexInitEncriptionClientToServer = map cipherName sshsEncriptionClientToServer
        , kexInitMacClientToServer = map macName sshsMacClientToServer
        , kexInitMacServerToClient = map macName sshsMacServerToClient
        , kexInitCompressionClientToServer = ["none"]
        , kexInitCompressionServerToClient = ["none"]
        , kexInitLanguagesClientToServer = []
        , kexInitLanguagesServerToClient = []
        , kexInitKexPacketFollows = packetFollows
        }

pipeline :: Conduit S.ByteString Ssh S.ByteString
pipeline = parser =$= packetPipeline =$= serializer
  where
    parser = do
      _ <- sinkGet parseHandshake
      forever $ do
        -- FIXME: using sinkGet here is bad, maybe, need to find better way
        lift packetParser >>= sinkGet >>= yield
--        liftIO $ threadDelay 2000000
        bumpInSeqNumber
    serializer = do
        ask >>= yield . runPut . serializeHandshake
        CL.mapM $ \packet -> do
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
