module Network.SSH.Client.Hssh.Packet where
import Data.Serialize.Get (Get, getWord32be, getWord8, getByteString, label)
import Data.Serialize.Put (Put, runPut, putByteString, putWord32be, putWord8)

import Data.ByteString as S
import Network.SSH.Client.Hssh.Prelude
import Network.SSH.Client.Hssh.Monad (Ssh, SshSettings(..), SshState(..))
import Network.SSH.Client.Hssh.Mac (Mac(..))

data Packet = Packet { packetPayload :: S.ByteString -- already decoded and unpacked data
                     }
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

parseHandshake :: Get S.ByteString
parseHandshake = do
    line <- crlfLine
    case sshDash `S.isPrefixOf` line of
      True -> parseVersionAndSoftware line
      False  -> fail "Bad handshake"
  where
    parseVersionAndSoftware line = do
      let (version, rest) = (S.splitAt 8 line)
      case version == sshTwoZeroDash of
        True -> return rest
        False -> fail "Unsupported SSH version"

sshDash :: S.ByteString
sshDash = "SSH-"

sshTwoZeroDash :: S.ByteString
sshTwoZeroDash = S.concat [sshDash, "2.0-"]

packetParser :: Ssh (Get Packet)
packetParser = do
    mac <- gets sshstMacServerToClient
    return $ parser mac
  where
    parser mac = do
      packetLength <- label "packet length" $ getWord32be
      paddingLength <- label "padding length" $ getWord8
      let payloadLength = fromIntegral $
              packetLength - (fromIntegral paddingLength) - 1

      payload <- label "payload" $ getByteString payloadLength
      padding <- label "padding" $ getByteString $ fromIntegral $ paddingLength
      signature <- label "mac" $ getByteString $ macLength mac
      -- Better way to concat Word32 and ByteString?
      let mySignature = calcMac mac $
              S.concat [runPut $ putWord32be packetLength, payload, padding]
      when (mySignature /= signature) $ fail "Bad MAC"
      return $ Packet { packetPayload = payload }

crlf :: S.ByteString
crlf = S.pack [13, 10]

serializeHandshake :: SshSettings -> Put
serializeHandshake SshSettings{sshsClientSoftware} = do
    putByteString (S.concat [sshTwoZeroDash, sshsClientSoftware, crlf])

packetSerializer :: Packet -> Ssh Put
packetSerializer Packet { .. } = do
    mac <- gets sshstMacClientToServer
    return $ do
      -- TODO: use cipher size to align in addition to default 8
      let paddingSize = 8 - (S.length packetPayload + 5) `rem` 8 + 8
          packetLength = fromIntegral $
              (S.length packetPayload) + paddingSize + 1
      putWord32be packetLength
      putWord8 $ fromIntegral paddingSize
      putByteString packetPayload
      let padding = S.pack [1..(fromIntegral paddingSize)]
      putByteString padding
      putByteString $ mac `calcMac` S.concat [ runPut $ putWord32be packetLength
                                             , packetPayload
                                             , padding ]
