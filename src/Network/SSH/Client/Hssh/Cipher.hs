module Network.SSH.Client.Hssh.Cipher where

import Data.ByteString as S

data Cipher = Cipher { cipherName :: S.ByteString }

none :: Cipher
none = Cipher { cipherName = "none" }

aes128_ctr :: Cipher
aes128_ctr = Cipher { cipherName = "aes128-ctr" }