import System.IO ( stdin, stdout, stderr
                 , hSetBuffering, BufferMode(LineBuffering))

import Network.SSH.Client.Hssh.Core (runClient)
import Network.SSH.Client.Hssh.Monad (SshSettings(..), defaultSettings)

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    hSetBuffering stdin LineBuffering
    hSetBuffering stderr LineBuffering
    runClient $ (defaultSettings "localhost") {sshsPort = 22 }
