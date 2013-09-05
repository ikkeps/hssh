module Network.SSH.Client.Hssh.Mac where

import Data.ByteString as S

import Crypto.Hash.CryptoAPI (MD5)

data Mac = Mac {
      macName :: S.ByteString
    , macLength :: Int
    , calcMac :: S.ByteString -> S.ByteString
    }

none :: Mac
none = Mac { macName = "none", macLength = 0, calcMac = const "" }

hmac_md5 :: Mac
hmac_md5 = Mac { macName = "hmac-md5", macLength = 0, calcMac = const "" }
