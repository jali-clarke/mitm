module Source (
    source
) where

import Data.Functor (void)

import Actor
import Daemon

source :: ActorContext m => m a -> [Mailbox m a] -> m ()
source generator mailboxes =
    void . loopDaemon $ do
        message <- generator
        traverse (flip writeMailbox message) mailboxes
