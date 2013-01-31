module Network.SSH.Client.Hssh.Messages where

import Data.Word (Word32, Word8)
import Data.ByteString.Lazy (ByteString, snoc, empty, fromStrict)
import qualified Data.ByteString.Lazy as BL
import Data.Serialize.Get ( Get, getWord32be, getWord8
                          , getByteString, skip, runGet, runGetLazy
                          , getLazyByteString )
import Data.Serialize.Put ( Put, putWord32be, putWord8
                          , putByteString
                          , putLazyByteString )
import Control.Monad (replicateM, forM)

import Network.SSH.Client.Hssh.Serialize


data SshMessage =
    Disconnect { disconnectReason :: Word32
               , disconnectDescription :: ByteString
               , disconnectLangTag :: ByteString }
  | Ignore ByteString
  | KexInit { kexInitLists            :: [[ByteString]]
            , kexInitKexPacketFollows :: Bool }
  deriving (Show)

parseDisconnect = do
    reason <- getWord32be
    description <- getString
    langCode <- getString
    return $ Disconnect reason description langCode


parseIgnore = getString >>= return . Ignore

parseKexInit = do
    _ <- getLazyByteString 16
    lists <- replicateM 10 parseList
    kexPacketFollows <- getBool
    -- getWord32be
    return $ KexInit lists kexPacketFollows

serialize (Ignore msg) = putString msg
serialize (KexInit lists kexPacketFollows) = do
    putLazyByteString $ BL.pack [1..16]
    forM lists putList
    putWord32be 0 -- ?
