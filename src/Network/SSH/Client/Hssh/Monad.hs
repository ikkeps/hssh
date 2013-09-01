module Network.SSH.Client.Hssh.Monad (Ssh, SshSettings(..), runSsh, defaultSettings) where

import qualified Data.ByteString as S
import Control.Monad (liftM)
import Control.Monad.Reader (ReaderT, MonadReader, runReaderT)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Control (MonadBaseControl(..))
import Control.Monad.Base (MonadBase(..))
import Control.Applicative (Applicative)
import Data.Conduit (MonadThrow)

data SshSettings = SshSettings { sshsHost :: S.ByteString
                               , sshsPort :: Int
                               , sshsSoftware :: S.ByteString }

newtype Ssh a = Ssh { unSsh :: ReaderT SshSettings IO a }
              deriving (MonadReader SshSettings, Monad, MonadIO, Applicative, MonadThrow, Functor)

instance MonadBase IO Ssh where
    liftBase = liftIO

instance MonadBaseControl IO Ssh where
    newtype StM Ssh a = SshStM { unSshStM :: StM (ReaderT SshSettings IO) a}
    liftBaseWith f = Ssh $ liftBaseWith $ \runInBase -> f $ liftM SshStM . runInBase . unSsh
    restoreM     = Ssh . restoreM . unSshStM

runSsh :: SshSettings -> Ssh a -> IO a
runSsh config m = runReaderT (unSsh m) config

defaultSettings :: S.ByteString -> SshSettings
defaultSettings host = SshSettings { sshsSoftware = "hssh"
                                   , sshsHost = host
                                   , sshsPort = 22 }
