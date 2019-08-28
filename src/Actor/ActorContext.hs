{-# LANGUAGE FlexibleContexts, TypeFamilies #-}

module Actor.ActorContext (
    ActorContext(..),
    ActorException(..)
) where

import qualified Control.Monad.Except as MTL

import Daemon

data ActorException = ActorTerminated | ActorTreeTerminated

class (MTL.MonadError ActorException m, DaemonContext m) => ActorContext m where
    type MailboxBase m :: * -> *
    
    newMailboxBase :: m (MailboxBase m a)

    getMessageBase :: MailboxBase m a -> m a
    putMessageBase :: a -> MailboxBase m a -> m ()
