module Network.SSH.Client.Hssh.Prelude ( (<*>)
                                       , (<>)
                                       , forever, forM, forM_, replicateM, when, foldM
                                       , (<$>)
                                       , Word32
                                       , ask, asks
                                       , get, gets, modify
                                       , lift
                                       , Word8
                                       , whenNothing) where

import Control.Applicative ((<*>))
import Control.Monad (forever, forM, forM_, foldM, replicateM, when)
import Control.Monad.Reader (ask, asks)
import Control.Monad.State (get, gets, modify)
import Control.Monad.Trans (lift)
import Data.Functor ((<$>))
import Data.Monoid ((<>))
import Data.Word (Word32, Word8)
import Data.Maybe (isNothing)

whenNothing :: Monad m => Maybe a -> m () -> m ()
whenNothing = when . isNothing
