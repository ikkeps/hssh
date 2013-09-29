module Network.SSH.Client.Hssh.Messages (SshMessage(..), getMessage, putMessage) where

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
  | KexDhInit 
  | Unsupported { unsupportedCode :: Word8 } -- FIXME: Add payload
  deriving (Show)

putMessage :: SshMessage -> Put
putMessage msg = do
    putWord8 $ codeOf msg
    putBody msg

putBody :: SshMessage -> Put
putBody (Ignore msg) = putString msg
putBody (KexInit {..}) = do
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
putBody (KexDhInit e) = putMpInt e
putBody Disconnect {..} = do
    putWord32be disconnectReason
    putString disconnectDescription
    putString disconnectLangTag
putBody Unsupported {..} = error "Trying to serialize unsupported message"

codeOf :: SshMessage -> Word8
codeOf (Disconnect {..}) = 1
codeOf (Ignore _) = 2
codeOf (KexInit {..}) = 20
codeOf (KexDhInit _) = 30
codeOf (Unsupported {..}) = error "Can not serialize code of unsupported message"

getMessage :: Get SshMessage
getMessage = do
    msgCode <- getWord8
    case msgCode of
       1 -> getDisconnect
       2 -> getIgnore
       20 -> getKexInit
       _  -> getUnsupported msgCode

getUnsupported :: Word8 -> Get SshMessage
getUnsupported msgCode = return $ Unsupported msgCode

getDisconnect :: Get SshMessage
getDisconnect = do
    reason <- getWord32be
    description <- getString
    langCode <- getString
    return $ Disconnect reason description langCode

getIgnore :: Get SshMessage
getIgnore = Ignore <$> getString

getKexInit :: Get SshMessage
getKexInit = do
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
