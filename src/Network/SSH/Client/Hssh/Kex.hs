module Network.SSH.Client.Hssh.Kex where

import Data.ByteString as S


data Kex = Kex { kexName :: S.ByteString
               -- I have no idea what to write here yet
               }

ecdh_sha2_nistp256 :: Kex
ecdh_sha2_nistp256 = Kex { kexName = "ecdh_sha2_nistp256" }