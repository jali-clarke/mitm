{-# LANGUAGE TypeFamilies #-}

module Actor (
    ActorContext(..),
    
    actorWithMailbox,
    actor,
    actorTerminal,
    forward
) where

import Data.Functor (void)
import qualified Control.Concurrent.Chan as C

import Daemon

class DaemonContext m => ActorContext m where
    type Mailbox m :: * -> *

    newMailbox :: m (Mailbox m a)
    readMailbox :: Mailbox m a -> m a
    writeMailbox :: Mailbox m a -> a -> m ()

instance ActorContext IO where
    type Mailbox IO = C.Chan

    newMailbox = C.newChan
    readMailbox = C.readChan
    writeMailbox = C.writeChan

actorWithMailbox :: ActorContext m => Mailbox m a -> [Mailbox m b] -> (a -> m b) -> m ()
actorWithMailbox source destinations callback =
    void . loopDaemon $ do
        inMessage <- readMailbox source
        outMessage <- callback inMessage
        traverse (flip writeMailbox outMessage) destinations

actor :: ActorContext m => [Mailbox m b] -> (a -> m b) -> m (Mailbox m a)
actor destinations callback =
    do
        source <- newMailbox
        source <$ actorWithMailbox source destinations callback

actorTerminal :: ActorContext m => (a -> m b) -> m (Mailbox m a)
actorTerminal = actor []

forward :: ActorContext m => [Mailbox m a] -> m (Mailbox m a)
forward destinations = actor destinations pure