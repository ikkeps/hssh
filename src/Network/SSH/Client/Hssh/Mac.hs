module Network.SSH.Client.Hssh.Mac where

import Data.ByteString as S

data Mac = Mac {
      macName :: S.ByteString
    , macLength :: Int
    , calcMac :: S.ByteString -> S.ByteString
    }

none :: Mac
none = Mac { macName = "none", macLength = 0, calcMac = const "" }

-- FIXME: ADD SUPPORT FOR HMAC-MD5 for fuck sake!
hmac_md5 :: Mac
hmac_md5 = Mac { macName = "hmac-md5", macLength = 0, calcMac = const "" }
