module Network.SSH.Client.Hssh.Messages where

import Data.ByteString as S
import Data.Serialize.Get ( Get, getWord32be, getWord8, getByteString )
import Data.Serialize.Put ( Put, putWord32be, putWord8, putByteString )

import Network.SSH.Client.Hssh.Prelude
import Network.SSH.Client.Hssh.ProtocolTypes


data SshMessage = forall msg. (SshMessageBinary msg) => SshMessage msg

class (Show msg) => SshMessageBinary msg where
    codeOf  :: msg -> Word8
    getBody :: Get msg
    putBody :: msg -> Put

data Disconnect = Disconnect { disconnectReason :: Word32
                             , disconnectDescription :: S.ByteString
                             , disconnectLangTag :: S.ByteString } deriving Show
data Ignore = Ignore S.ByteString deriving Show
data KexInit = KexInit { kexInitAlgorithms                :: [S.ByteString]
                       , kexInitServerHostKeyAlgorithms   :: [S.ByteString]
                       , kexInitEncriptionOut  :: [S.ByteString]
                       , kexInitEncriptionIn  :: [S.ByteString]
                       , kexInitMacOut         :: [S.ByteString]
                       , kexInitMacIn         :: [S.ByteString]
                       , kexInitCompressionOut :: [S.ByteString]
                       , kexInitCompressionIn :: [S.ByteString]
                       , kexInitLanguagesOut   :: [S.ByteString]
                       , kexInitLanguagesIn   :: [S.ByteString]
                       , kexInitKexPacketFollows :: Bool } deriving (Show)
-- FIXME: it's just a placeholder!
data KexDhInit = KexDhInit { kexDhInitE :: Integer } deriving (Show)
-- FIXME: Add payload
data Unsupported = Unsupported { unsupportedCode :: Word8 } deriving (Show)


putMessage :: SshMessage -> Put
putMessage (SshMessage msg) = do
    putWord8 $ codeOf msg
    putBody msg

instance SshMessageBinary Ignore where
    codeOf = const 2
    putBody (Ignore msg) = putString msg
    getBody = Ignore <$> getString

instance SshMessageBinary KexInit where
    codeOf = const 20
    getBody = do
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
        putWord32be 0 -- reserved

instance SshMessageBinary Disconnect where
    codeOf = const 1
    putBody Disconnect {..} = do
        putWord32be disconnectReason
        putString disconnectDescription
        putString disconnectLangTag
    getBody = do
        reason <- getWord32be
        description <- getString
        langCode <- getString
        return $ Disconnect reason description langCode



getMessage :: Get SshMessage
getMessage = do
    msgCode <- getWord8
    -- This is a shame on Haskell. Why case is not polymophic?
    case msgCode of
       1  -> SshMessage <$> (getBody :: Get Disconnect)
       2  -> SshMessage <$> (getBody :: Get Ignore)
       20 -> SshMessage <$> (getBody :: Get KexInit)
       _  -> error "fail!"
