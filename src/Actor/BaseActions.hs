module Actor.BaseActions (
    Mailbox,

    newMailbox,

    getMessage,
    putMessage,

    signalStop,
    signalStopAll,

    propagateException
) where

import qualified Control.Monad.Except as MTL
import Data.Foldable (traverse_)

import Actor.ActorContext

data MailboxMessage a = StopActor | StopActorTree | Message a
newtype Mailbox m a = Mailbox (MailboxBase m (MailboxMessage a))

newMailbox :: ActorContext m => m (Mailbox m a)
newMailbox = fmap Mailbox newMailboxBase

getMessage :: ActorContext m => Mailbox m a -> m a
getMessage (Mailbox mailboxBase) = do
    message <- getMessageBase mailboxBase
    case message of
        StopActor -> MTL.throwError ActorTerminated
        StopActorTree -> MTL.throwError ActorTreeTerminated
        Message message' -> pure message'

putMessage :: ActorContext m => a -> Mailbox m a -> m ()
putMessage message (Mailbox mailboxBase) = putMessageBase (Message message) mailboxBase

signalStop :: ActorContext m => Mailbox m a -> m ()
signalStop (Mailbox mailboxBase) = putMessageBase StopActor mailboxBase

signalStopAll :: ActorContext m => Mailbox m a -> m ()
signalStopAll (Mailbox mailboxBase) = putMessageBase StopActorTree mailboxBase

propagateException :: ActorContext m => [Mailbox m a] -> m () -> m ()
propagateException childMailboxes action =
    let handler exception =
            case exception of
                ActorTerminated -> pure ()
                ActorTreeTerminated -> traverse_ signalStopAll childMailboxes
    in action `MTL.catchError` handler