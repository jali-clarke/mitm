{-# LANGUAGE TypeFamilies #-}

module Actor (
    ActorContext(..),
    
    registerSourceTopLevel,
    registerSource,
    registerActor,
    registerActorWithMailbox,
    registerTerminal
) where

import Control.Monad (forever)
import qualified Control.Concurrent.Chan as C

import Daemon

class DaemonContext m => ActorContext m where
    type Mailbox m :: * -> *
    
    newMailbox :: m (Mailbox m a)

    getMessage :: Mailbox m a -> m a
    putMessage :: a -> Mailbox m a -> m ()

instance ActorContext IO where
    type Mailbox IO = C.Chan

    newMailbox = C.newChan

    getMessage = C.readChan
    putMessage = flip C.writeChan

registerSourceTopLevel :: ActorContext m => m a -> [Mailbox m a] -> m ()
registerSourceTopLevel action targets = forever $ do
    message <- action
    traverse (putMessage message) targets

registerSource :: ActorContext m => m a -> [Mailbox m a] -> m ()
registerSource action targets = daemon $ registerSourceTopLevel action targets

registerActorWithMailbox :: ActorContext m => Mailbox m a -> [Mailbox m b] -> (a -> m b) -> m ()
registerActorWithMailbox mailbox targets callback = registerSource (getMessage mailbox >>= callback) targets

registerActor :: ActorContext m => [Mailbox m b] -> (a -> m b) -> m (Mailbox m a)
registerActor targets callback = do
    mailbox <- newMailbox
    mailbox <$ registerActorWithMailbox mailbox targets callback

registerTerminal :: ActorContext m => (a -> m b) -> m (Mailbox m a)
registerTerminal = registerActor []