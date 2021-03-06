module Network.SSH.Client.Hssh.ProtocolTypes where

import qualified Data.ByteString as S
import Data.Char (ord)
import Data.Serialize.Get ( Get, getWord32be, getWord8, getByteString )
import Data.Serialize.Put ( Put, putWord32be, putWord8, putByteString )
import Control.Applicative ((<$>))

import Crypto.Util (i2bs_unsized)

getBool :: Get Bool
getBool = (/= 0) <$> getWord8

putBool :: Bool -> Put
putBool True  = putWord8 1
putBool False = putWord8 0

getString :: Get S.ByteString
getString = getWord32be >>= getByteString . fromIntegral

putString :: S.ByteString -> Put
putString str = do
    putWord32be $ fromIntegral $ S.length str
    putByteString str

putMpInt :: Integer -> Put
putMpInt 0 = putString ""
putMpInt i | i > 0 = putString $ i2bs_unsized i
           | i < 0 = error "Negative mpint not implemented!"


getList :: Get [S.ByteString]
getList = do
    string <- getString
    return $ S.split (fromIntegral $ ord ',') string

putList :: [S.ByteString] -> Put
putList strings =
      putString $ S.intercalate (S.pack [fromIntegral $ ord ',']) strings