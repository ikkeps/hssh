import Data.Conduit
import Data.Conduit.Util (sequenceSink, SequencedSinkResponse(..))
import Data.Conduit.Binary as CB
import Data.Conduit.Cereal (conduitGet, sinkGet, conduitPut)
import System.IO ( stdin, stdout, stderr
                 , hPutStrLn, hSetBuffering, BufferMode(NoBuffering))
import Data.Int (Int64)
import qualified Data.ByteString.Char8 as S
import Control.Monad.IO.Class (liftIO, MonadIO)

import Control.Concurrent (threadDelay)

import Network.SSH.Client.Hssh.Core


crlfLine :: Get ByteString
crlfLine = go empty
  where go string = do
          ch <- getWord8
          case ch of
            13 -> do
              _ <- getWord8
              return string
            _  -> go (snoc string ch)


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
    _mac <- readMac
    return $ Packet payload
  where
    readMac = case withMac of
        True -> Just <$> getLazyByteString 8
        False -> return Nothing


crlf :: ByteString
crlf = BL.pack [13, 10]

serializePacket (Handshake software) = do
    putLazyByteString (BL.concat [sshTwoZeroDash, software, crlf])
serializePacket (Packet rawData) = do
    let paddingSize = 37
    putWord32be $ (BL.length rawData) + 37
    putWord8 paddingSize
    putLazyByteString rawData
    putLazyByteString $ S.pack [1..paddingSize]


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
    hSetBuffering stdout NoBuffering
    hSetBuffering stdin NoBuffering
    runResourceT $ CB.sourceHandle stdin $= pipeline $$ CB.sinkHandle stdout
