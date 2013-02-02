module Network.SSH.Client.Hssh.Packet where

import Data.ByteString.Lazy (ByteString, snoc, empty, fromStrict)
import qualified Data.ByteString.Lazy as BL
import Data.Conduit.Binary as CB
import qualified Data.ByteString.Char8 as S
import Data.Serialize.Get ( Get, getWord32be, getWord8
                          , getByteString, skip, runGet, runGetLazy
                          , getLazyByteString )
import Data.Serialize.Put ( Put, runPut, putLazyByteString, putByteString
                          , putWord32be, putWord8 )
import Control.Applicative ((<$>))


data Packet = Handshake { handshakeSoftware :: ByteString }
            | Packet ByteString -- already decoded and unpacked data
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

    payload <- getLazyByteString payloadLength
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
    putWord32be $ fromIntegral $ (BL.length rawData) + paddingSize
    putWord8 $ fromIntegral paddingSize
    putLazyByteString rawData
    putLazyByteString $ BL.pack [1..(fromIntegral paddingSize)]
