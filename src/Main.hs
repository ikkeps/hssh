import System.IO ( stdin, stdout, stderr
                 , hSetBuffering, BufferMode(NoBuffering))

import Network.SSH.Client.Hssh.Core (runClient)
import Network.SSH.Client.Hssh.Monad (SshSettings(..), defaultSettings)

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    hSetBuffering stdin NoBuffering
    hSetBuffering stderr NoBuffering
    runClient $ (defaultSettings "localhost") {sshsPort = 12345 }
