module Daemon (
    DaemonContext(..)
) where

import Data.Functor (void)
import qualified Control.Concurrent as C

class Monad m => DaemonContext m where
    daemon :: m a -> m ()

instance DaemonContext IO where
    daemon action = void $ C.forkIO (void action)