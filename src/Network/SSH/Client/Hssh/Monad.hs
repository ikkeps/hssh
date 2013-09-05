module Network.SSH.Client.Hssh.Monad (Ssh
                                     , SshSettings(..), SshState(..)
                                     , runSsh, defaultSettings) where

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

import qualified Network.SSH.Client.Hssh.Mac as Mac
import qualified Network.SSH.Client.Hssh.Kex as Kex
import qualified Network.SSH.Client.Hssh.Cipher as Cipher

data SshSettings = SshSettings { sshsHost :: S.ByteString
                               , sshsPort :: Int
                               , sshsClientSoftware :: S.ByteString
                                 -- Available algorithms in preference order
                               , sshsKexAlgorithms :: [Kex.Kex]
                               , sshsEncriptionOut :: [Cipher.Cipher]
                               , sshsEncriptionIn  :: [Cipher.Cipher]
                               , sshsMacOut        :: [Mac.Mac]
                               , sshsMacIn         :: [Mac.Mac]
                                 -- FIXME: add compression?
--                               , sshsCompressionOut :: [Compression]
--                               , sshsCompressionIn :: [Compression]
                                 -- No support for languages list. Nobody cares.
                               }

data SshState = SshState { sshstInputSeqNumber  :: Word32
                         , sshstOutputSeqNumber :: Word32
                         , sshstKexAlgorithm  :: Kex.Kex
                         , sshstEncriptionOut :: Cipher.Cipher
                         , sshstEncriptionIn  :: Cipher.Cipher
                         , sshstMacOut        :: Mac.Mac
                         , sshstMacIn         :: Mac.Mac
                           -- FIXME: add compression?
--                         , sshsCompressionOut :: Compression
--                         , sshsCompressionIn :: Compression
                         }

-- TODO: add error transformer somewhere in monad stack?
newtype Ssh a = Ssh { unSsh :: ReaderT SshSettings (StateT SshState IO) a }
              deriving (MonadReader SshSettings, Monad, MonadState SshState, MonadIO, Applicative, MonadThrow, Functor)

instance MonadBase IO Ssh where
    liftBase = liftIO

instance MonadBaseControl IO Ssh where
    newtype StM Ssh a = SshStM { unSshStM :: StM (ReaderT SshSettings (StateT SshState IO)) a}
    liftBaseWith f = Ssh $ liftBaseWith $ \runInBase -> f $ liftM SshStM . runInBase . unSsh
    restoreM = Ssh . restoreM . unSshStM

runSsh :: SshSettings -> Ssh a -> IO a
runSsh config m = evalStateT (runReaderT (unSsh m) config) initialState

initialState :: SshState
initialState = SshState { sshstInputSeqNumber = 0
                        , sshstOutputSeqNumber = 0
                        , sshstKexAlgorithm = Kex.ecdh_sha2_nistp256
                        , sshstMacOut = Mac.none
                        , sshstMacIn = Mac.none
                        , sshstEncriptionOut = Cipher.none
                        , sshstEncriptionIn = Cipher.none }


defaultSettings :: S.ByteString -> SshSettings
defaultSettings host =
    SshSettings { sshsClientSoftware = "hssh"
                , sshsHost = host
                , sshsPort = 22
                , sshsKexAlgorithms = [Kex.ecdh_sha2_nistp256]
                , sshsEncriptionOut = [Cipher.aes128_ctr]
                , sshsEncriptionIn = [Cipher.aes128_ctr]
                , sshsMacOut = [Mac.hmac_md5, Mac.none]
                , sshsMacIn = [Mac.hmac_md5, Mac.none] }
