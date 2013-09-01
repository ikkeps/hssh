module Network.SSH.Client.Hssh.Monad (Ssh, SshSettings(..), SshState(..), runSsh, defaultSettings) where

import Data.Word (Word32)
import qualified Data.ByteString as S
import Control.Monad (liftM)
import Control.Monad.Reader (ReaderT, MonadReader, runReaderT)
import Control.Monad.State (StateT, MonadState, evalStateT)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Control (MonadBaseControl(..))
import Control.Monad.Base (MonadBase(..))
import Control.Applicative (Applicative)
import Data.Conduit (MonadThrow)

import Network.SSH.Client.Hssh.Mac

data SshSettings = SshSettings { sshsHost :: S.ByteString
                               , sshsPort :: Int
                               , sshsSoftware :: S.ByteString }

data SshState = SshState { sshstMac :: Mac
                         , sshstInputSeqNumber  :: Word32
                         , sshstOutputSeqNumber :: Word32 }

newtype Ssh a = Ssh { unSsh :: ReaderT SshSettings (StateT SshState IO) a }
              deriving (MonadReader SshSettings, Monad, MonadState SshState, MonadIO, Applicative, MonadThrow, Functor)

instance MonadBase IO Ssh where
    liftBase = liftIO

instance MonadBaseControl IO Ssh where
    newtype StM Ssh a = SshStM { unSshStM :: StM (ReaderT SshSettings (StateT SshState IO)) a}
    liftBaseWith f = Ssh $ liftBaseWith $ \runInBase -> f $ liftM SshStM . runInBase . unSsh
    restoreM = Ssh . restoreM . unSshStM

runSsh :: SshSettings -> Ssh a -> IO a
runSsh config m = evalStateT (runReaderT (unSsh m) config) (SshState mkMacNone 0 0)

defaultSettings :: S.ByteString -> SshSettings
defaultSettings host = SshSettings { sshsSoftware = "hssh"
                                   , sshsHost = host
                                   , sshsPort = 22 }
