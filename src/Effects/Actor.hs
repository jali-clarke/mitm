{-# LANGUAGE TypeFamilies #-}

module Effects.Actor where

import qualified Data.ByteString.Lazy as Bytes

class Monad m => Actor m where
    type Mailbox m :: *

    newMailbox :: m (Mailbox m)

    writeMessage :: Mailbox m -> Bytes.ByteString -> m ()
    readMessage :: Mailbox m -> m Bytes.ByteString