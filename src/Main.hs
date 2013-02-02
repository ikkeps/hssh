import Data.Conduit
import Data.Conduit.Binary (sourceHandle, sinkHandle)
import System.IO ( stdin, stdout, stderr
                 , hSetBuffering, BufferMode(NoBuffering))

import Network.SSH.Client.Hssh.Core (pipeline)

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    hSetBuffering stdin NoBuffering
    hSetBuffering stderr NoBuffering
    runResourceT $ sourceHandle stdin $= pipeline $$ sinkHandle stdout
