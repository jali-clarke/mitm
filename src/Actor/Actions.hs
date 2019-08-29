module Actor.Actions (
    registerTopLevelSource,
    registerSource,
    registerActorWithMailbox,
    registerActor,
    registerTerminal,

    noResource
) where

import Control.Monad (forever)
import Data.Foldable (traverse_)
import Data.Functor (void)

import Actor.ActorContext
import Actor.BaseActions
import Daemon

registerTopLevelSource :: ActorContext m => m a -> (a -> m b) -> (a -> m c) -> [Mailbox m b] -> m ()
registerTopLevelSource initializer action cleanup targets = do
    resource <- initializer
    propagateException targets . forever $ do
        message <- action resource
        traverse_ (putMessage message) targets
    void (cleanup resource)

registerSource :: ActorContext m => m a -> (a -> m b) -> (a -> m c) -> [Mailbox m b] -> m ()
registerSource initializer action cleanup targets = daemon $ registerTopLevelSource initializer action cleanup targets

registerActorWithMailbox :: ActorContext m => m a -> (a -> b -> m c) -> (a -> m d) -> Mailbox m b -> [Mailbox m c] -> m ()
registerActorWithMailbox initializer action cleanup mailbox targets =
    let sourceAction resource = getMessage mailbox >>= action resource
    in registerSource initializer sourceAction cleanup targets

registerActor :: ActorContext m => m a -> (a -> b -> m c) -> (a -> m d) -> [Mailbox m c] -> m (Mailbox m b)
registerActor initializer action cleanup targets = do
    mailbox <- newMailbox
    mailbox <$ registerActorWithMailbox initializer action cleanup mailbox targets

registerTerminal :: ActorContext m => m a -> (a -> b -> m c) -> (a -> m d) -> m (Mailbox m b)
registerTerminal initializer action cleanup = registerActor initializer action cleanup []

noResource :: ActorContext m => (m () -> (a -> b) -> (a -> m ()) -> d) -> b -> d
noResource registrar action = registrar (pure ()) (const action) (const $ pure ())