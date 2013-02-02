module Network.SSH.Client.Hssh.ProtocolTypes where

import Data.ByteString.Lazy ( ByteString, snoc, empty, fromStrict )
import qualified Data.ByteString.Lazy as BL
import Data.Char (ord)
import Data.Serialize.Get ( Get, getWord32be, getWord8
                          , getByteString, skip, runGet, runGetLazy
                          , getLazyByteString )
import Data.Serialize.Put ( Put, putWord32be, putWord8
                          , putByteString
                          , putLazyByteString )
import Control.Applicative ((<$>))

getBool :: Get Bool
getBool = (/= 0) <$> getWord8

putBool :: Bool -> Put
putBool True  = putWord8 1
putBool False = putWord8 0

getString :: Get ByteString
getString = getWord32be >>= getLazyByteString . fromIntegral

putString :: ByteString -> Put
putString str = do
    putWord32be $ fromIntegral $ BL.length str
    putLazyByteString str

parseList :: Get [ByteString]
parseList = do
    string <- getString
    return $ BL.split (fromIntegral $ ord ',') string

putList :: [ByteString] -> Put
putList strings =
      putString $ BL.intercalate (BL.pack [fromIntegral $ ord ',']) strings