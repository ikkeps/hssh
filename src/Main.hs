import Data.Conduit
import Data.Conduit.Binary (sourceHandle, sinkHandle)
import Data.Conduit.Network ( Application, clientSettings, runTCPClient
                            , appSource, appSink )
import System.IO ( stdin, stdout, stderr
                 , hSetBuffering, BufferMode(NoBuffering))

import Network.SSH.Client.Hssh.Core (pipeline)

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    hSetBuffering stdin NoBuffering
    hSetBuffering stderr NoBuffering
    runTCPClient (clientSettings 22 "localhost") app

app :: Application IO
app d = (appSource d) $= pipeline $$ (appSink d)