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
            , kexInitEncriptionClientToServer  :: [S.ByteString]
            , kexInitEncriptionServerToClient  :: [S.ByteString]
            , kexInitMacClientToServer         :: [S.ByteString]
            , kexInitMacServerToClient         :: [S.ByteString]
            , kexInitCompressionClientToServer :: [S.ByteString]
            , kexInitCompressionServerToClient :: [S.ByteString]
            , kexInitLanguagesClientToServer   :: [S.ByteString]
            , kexInitLanguagesServerToClient   :: [S.ByteString]
            , kexInitKexPacketFollows :: Bool }
  | KexDhInit { kexDhInitE :: Integer }
  deriving (Show)

serialize :: SshMessage -> Put
serialize (Ignore msg) = putWord8 2 >> putString msg
serialize (KexInit {..}) = do
    putWord8 20
    putByteString $ S.pack [1..16] -- cookie
    mapM putList [ kexInitAlgorithms
                 , kexInitServerHostKeyAlgorithms
                 , kexInitEncriptionClientToServer
                 , kexInitEncriptionServerToClient
                 , kexInitMacClientToServer
                 , kexInitMacServerToClient
                 , kexInitCompressionClientToServer
                 , kexInitCompressionServerToClient
                 , kexInitLanguagesClientToServer
                 , kexInitLanguagesServerToClient ]
    putBool kexInitKexPacketFollows
    putWord32be 0 -- ?
serialize (KexDhInit e) = do
    putWord8 30 -- Who can find this constant value in less than 10 minutes?
    putMpInt e
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
    _cookie <- getByteString 16
    msg <- KexInit
            <$> getList
            <*> getList
            <*> getList
            <*> getList
            <*> getList
            <*> getList
            <*> getList
            <*> getList
            <*> getList
            <*> getList
            <*> getBool
    _reserved <- getWord32be
    return msg
