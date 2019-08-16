{-# LANGUAGE TypeFamilies #-}

module Daemon (
    DaemonContext(..)
) where

import qualified Control.Concurrent as C

class Monad m => DaemonContext m where
    type Daemon m :: *
    daemon :: m () -> m (Daemon m)

instance DaemonContext IO where
    type Daemon IO = C.ThreadId
    daemon action = C.forkIO action