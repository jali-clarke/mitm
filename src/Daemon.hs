module Daemon (
    DaemonContext(..)
) where

class Monad m => DaemonContext m where
    daemon :: m a -> m ()