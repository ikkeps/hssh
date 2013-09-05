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
  | KexInit { kexInitAlgorithms                :: [S.ByteString]
            , kexInitServerHostKeyAlgorithms   :: [S.ByteString]
            , kexInitEncriptionOut  :: [S.ByteString]
            , kexInitEncriptionIn  :: [S.ByteString]
            , kexInitMacOut         :: [S.ByteString]
            , kexInitMacIn         :: [S.ByteString]
            , kexInitCompressionOut :: [S.ByteString]
            , kexInitCompressionIn :: [S.ByteString]
            , kexInitLanguagesOut   :: [S.ByteString]
            , kexInitLanguagesIn   :: [S.ByteString]
            , kexInitKexPacketFollows :: Bool }
  | KexDhInit { kexDhInitE :: Integer }
  deriving (Show)

serialize :: SshMessage -> Put
serialize msg = do
    putWord8 $ code msg
    serializeBody msg

serializeBody :: SshMessage -> Put
serializeBody (Ignore msg) = putString msg
serializeBody (KexInit {..}) = do
    putByteString $ S.pack [1..16] -- cookie
    mapM_ putList [ kexInitAlgorithms
                  , kexInitServerHostKeyAlgorithms
                  , kexInitEncriptionOut
                  , kexInitEncriptionIn
                  , kexInitMacOut
                  , kexInitMacIn
                  , kexInitCompressionOut
                  , kexInitCompressionIn
                  , kexInitLanguagesOut
                  , kexInitLanguagesIn ]
    putBool kexInitKexPacketFollows
    putWord32be 0 -- ?
serializeBody (KexDhInit e) = putMpInt e
serializeBody (Disconnect {..}) = do
    putWord32be disconnectReason
    putString disconnectDescription
    putString disconnectLangTag

code :: SshMessage -> Word8
code (Disconnect {..}) = 1
code (Ignore _) = 2
code (KexInit {..}) = 20
code (KexDhInit _) = 30

parse :: Get SshMessage
parse = do
    msgCode <- getWord8
    case msgCode of
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
    _cookie <- getByteString 16
    kexInitAlgorithms <- getList
    kexInitServerHostKeyAlgorithms <- getList
    kexInitEncriptionOut <- getList
    kexInitEncriptionIn <- getList
    kexInitMacOut <- getList
    kexInitMacIn <- getList
    kexInitCompressionOut <- getList
    kexInitCompressionIn <- getList
    kexInitLanguagesOut <- getList
    kexInitLanguagesIn <- getList
    kexInitKexPacketFollows <- getBool
    _reserved <- getWord32be
    return $ KexInit {..}
