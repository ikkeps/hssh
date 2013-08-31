module Network.SSH.Client.Hssh.Packet where

import qualified Data.ByteString as S
import Data.Serialize.Get (Get, getWord32be, getWord8, getByteString, skip)
import Data.Serialize.Put (Put, putByteString, putWord32be, putWord8)
import Control.Applicative ((<$>))


data Packet = Handshake { handshakeSoftware :: S.ByteString }
            | Packet { packetPayload :: S.ByteString -- already decoded and unpacked data
                     , packetMac     :: Maybe S.ByteString }
                deriving (Show)


crlfLine :: Get S.ByteString
crlfLine = go S.empty
  where go string = do
          ch <- getWord8
          case ch of
            13 -> do
              _ <- getWord8
              return string
            _  -> go (S.snoc string ch)

handshake :: Get Packet
handshake = do
    line <- crlfLine
    case sshDash `S.isPrefixOf` line of
      True -> parseVersionAndSoftware line
      False  -> handshake
  where
    parseVersionAndSoftware line = do
      let (version, rest) = (S.splitAt 8 line)
      case version == sshTwoZeroDash of
        True -> return $! Handshake {handshakeSoftware = rest}
        False -> fail "Unsupported SSH version"

sshDash :: S.ByteString
sshDash = "SSH-"

sshTwoZeroDash :: S.ByteString
sshTwoZeroDash = S.concat [sshDash, "2.0-"]

parsePacket :: Bool -> Get Packet
parsePacket withMac = do
    packetLength <- getWord32be
    paddingLength <- getWord8
    let payloadLength = (fromIntegral packetLength) - (fromIntegral paddingLength) - 1

    payload <- getByteString payloadLength
    skip $ fromIntegral $ paddingLength
    mac <- readMac
    return $ Packet { packetPayload = payload
                    , packetMac = mac }
  where
    readMac = case withMac of
        True -> Just <$> getByteString 8
        False -> return Nothing


crlf :: S.ByteString
crlf = S.pack [13, 10]

serializePacket :: Packet -> Put
serializePacket (Handshake software) = do
    putByteString (S.concat [sshTwoZeroDash, software, crlf])
serializePacket Packet { .. } = do
    let paddingSize = 45 -- TODO: 8 or cipher size align
    let len = (S.length packetPayload) + paddingSize + 1
    putWord32be $ fromIntegral len
    putWord8 $ fromIntegral paddingSize
    putByteString packetPayload
    putByteString $ S.pack [1..(fromIntegral paddingSize)]
    -- TODO: MAC
