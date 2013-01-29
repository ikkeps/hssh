import Data.Conduit
import Data.Conduit.Util (sequenceSink, SequencedSinkResponse(..))
import Data.Conduit.Binary as CB
import Data.Conduit.Cereal (conduitGet, sinkGet, conduitPut)
import System.IO (stdin, stdout, stderr, hPutStrLn)
import Data.Serialize.Get ( Get, getWord32be, getWord8
                          , getByteString, skip, runGet, runGetLazy
                          , getLazyByteString )
import Data.Serialize.Put ( Put, putWord32be, putWord8
                          , putByteString
                          , putLazyByteString )
import Data.Word (Word32, Word8)
import Data.Int (Int64)
import Data.ByteString.Lazy ( ByteString, snoc, empty, fromStrict )
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as S
import Control.Monad (replicateM)
import Control.Monad.IO.Class (liftIO, MonadIO)

import Control.Applicative ((<$>), (<*>), (<*))

data Packet = Packet { packetLength        :: Word32
                     , packetPaddingLength :: Word8
                     , packetMsg           :: Either String SshMsg
                     , packetMac           :: Maybe ByteString }
            | Handshake { handshakeSoftware :: ByteString }
                deriving (Show)

data SshMsg =
    SshMsgIgnore
  | SshMsgKexInit { msgKexInitLists       :: [[ByteString]]
                  , firstKexPacketFollows :: Bool }
  | SshMsgDisconnect { msgDisconnectReason      :: Word32
                     , msgDisconnectDescription :: ByteString
                     , msgDisconnectLangTag     :: ByteString }
        deriving (Show)


crlfLine :: Get ByteString
crlfLine = go empty
  where go string = do
          ch <- getWord8
          case ch of
            13 -> do
              _ <- getWord8
              return string
            _  -> go (snoc string ch)

getString :: Get ByteString
getString = do
    getWord32be >>= getLazyByteString . fromIntegral

handshake :: Get Packet
handshake = do
    line <- crlfLine
    case sshDash `BL.isPrefixOf` line of
      True -> parseVersionAndSoftware line
      False  -> handshake
  where
    parseVersionAndSoftware line = do
      let (version, rest) = (BL.splitAt 8 line)
      case version == sshTwoZeroDash of
        True -> return $! Handshake {handshakeSoftware = rest}
        False -> fail "Unsupported SSH version"

sshDash :: ByteString
sshDash = fromStrict $ S.pack "SSH-"

sshTwoZeroDash :: ByteString
sshTwoZeroDash = BL.concat [sshDash, fromStrict $ S.pack "2.0-"]

parsePacket :: Bool -> Get Packet
parsePacket withMac = do
    packetLength <- getWord32be
    paddingLength <- getWord8
    let payloadLength = (fromIntegral packetLength) - (fromIntegral paddingLength) - 1

    payload <- getByteString payloadLength
    skip $ fromIntegral $ paddingLength
    mac <- readMac
    return $!
      Packet packetLength paddingLength (runGet parseSshMsg payload)  mac
  where
    readMac = case withMac of
        True -> Just <$> getLazyByteString 8
        False -> return Nothing

parseDisconnect :: Get SshMsg
parseDisconnect = do
    code <- getWord32be
    description <- getString
    langCode <- getString
    return $ SshMsgDisconnect code description langCode

getBool :: Get Bool
getBool = (/= 0) <$> getWord8

parseKexInit :: Get SshMsg
parseKexInit =
    return SshMsgKexInit
                  <*  getLazyByteString 16
                  <*> replicateM 10 parseList
                  <*> getBool
                  <*  getWord32be

parseIgnore :: Get SshMsg
parseIgnore = return SshMsgIgnore

parseList :: Get [ByteString]
parseList = do
    string <- getString
    return $ BL.split 44 string -- ','

parseSshMsg :: Get SshMsg
parseSshMsg = do
    code <- getWord8
    case code of
       1 -> parseDisconnect -- SSH_MSG_DISONNECT
       20 -> parseKexInit    -- SSH_MSG_KEX_INIT
       2 -> parseIgnore     -- SSH_MSG_IGNORE
       4 -> parseIgnore     -- SSH_MSG_DEBUG
       _  -> fail $ "unknown code: " ++ show code

crlf :: ByteString
crlf = BL.pack [13, 10]

serializeSshMsg :: SshMsg -> Put
serializeSshMsg (SshMsgKexInit lists kexFollows) =
  putByteString $ S.pack "NOT_IMPLEMENTED"

serializePacket :: Packet -> Put
serializePacket (Handshake software) = do
    putLazyByteString (BL.concat [sshTwoZeroDash, software, crlf])
serializePacket (Packet len paddingLength (Right msg) mbMac) = do
    putWord32be len
    putWord8    paddingLength
    serializeSshMsg msg
    case mbMac of
      Just mac -> putLazyByteString mac
      Nothing -> return ()


debug :: (MonadIO m) => String -> m ()
debug = liftIO . (hPutStrLn stderr)

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
                yield $ packet
                go
          Nothing -> return ()


pipeline :: (MonadIO m, MonadThrow m) => Conduit S.ByteString m S.ByteString
pipeline = parser =$= core =$= serializer
  where
    parser = do
      sinkGet handshake >>= yield
      conduitGet $ parsePacket False
    serializer = do
      conduitPut serializePacket

main :: IO ()
main = do
    runResourceT $ CB.sourceHandle stdin $= pipeline $$ CB.sinkHandle stdout
