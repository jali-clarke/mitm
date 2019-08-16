{-# LANGUAGE TypeFamilies #-}

module Daemon (
    DaemonContext(..),
    loopDaemon
) where

import Data.Functor (void)
import Control.Monad (forever)
import qualified Control.Concurrent as C

class Monad m => DaemonContext m where
    type Daemon m :: *
    daemon :: m a -> m (Daemon m)

instance DaemonContext IO where
    type Daemon IO = C.ThreadId
    daemon action = C.forkIO $ void action

loopDaemon :: DaemonContext m => m a -> m (Daemon m)
loopDaemon action = daemon (forever action)