module Network.SSH.Client.Hssh.Messages where

import Data.ByteString as S
import Data.Serialize.Get ( Get, getWord32be, getWord8, getByteString )
import Data.Serialize.Put ( Put, putWord32be, putWord8, putByteString )

import Network.SSH.Client.Hssh.Prelude
import Network.SSH.Client.Hssh.ProtocolTypes


data SshMessage =
    Disconnect { disconnectReason :: Word32
               , disconnectDescription :: S.ByteString
               , disconnectLangTag :: S.ByteString }
  | Ignore S.ByteString
  | KexInit { kexInitLists            :: [[S.ByteString]]
            , kexInitKexPacketFollows :: Bool }
  deriving (Show)

serialize :: SshMessage -> Put
serialize (Ignore msg) = putWord8 2 >> putString msg
serialize (KexInit lists _kexPacketFollows) = do
    putWord8 20
    putByteString $ S.pack [1..16]
    forM_ lists putList
    putWord32be 0 -- ?
serialize (Disconnect {..}) = do
    putWord8 1
    putWord32be disconnectReason
    putString disconnectDescription
    putString disconnectLangTag

parse :: Get SshMessage
parse = do
    code <- getWord8
    case code of
       1 -> parseDisconnect
       2 -> parseIgnore
       20 -> parseKexInit
       _  -> parseIgnore

parseDisconnect :: Get SshMessage
parseDisconnect = do
    reason <- getWord32be
    description <- getString
    langCode <- getString
    return $ Disconnect reason description langCode

parseIgnore :: Get SshMessage
parseIgnore = getString >>= return . Ignore

parseKexInit :: Get SshMessage
parseKexInit = do
    _ <- getByteString 16
    lists <- replicateM 10 parseList
    kexPacketFollows <- getBool
    -- getWord32be
    return $ KexInit lists kexPacketFollows
