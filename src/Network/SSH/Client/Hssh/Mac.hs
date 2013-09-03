module Network.SSH.Client.Hssh.Mac where

import Data.ByteString as S

import Crypto.Hash.CryptoAPI (MD5)

import Network.SSH.Client.Hssh.Prelude

data Mac = Mac {
      macName :: S.ByteString
    , macLength :: Int
    , calcMac :: S.ByteString -> S.ByteString
    }


mkMacNone = Mac { macName = "none", macLength = 0, calcMac = const "" }
