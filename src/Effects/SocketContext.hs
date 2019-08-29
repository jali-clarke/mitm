{-# LANGUAGE TypeFamilies #-}

module Effects.SocketContext (
    SocketContext(..)
) where

import qualified Data.ByteString as B

class Monad m => SocketContext m where
    type CommunicatorSocket m :: *
    type ListenSocket m :: *
    type SocketInfo m :: *

    listenAsServer :: String -> m (ListenSocket m)
    acceptConnection :: ListenSocket m -> m (CommunicatorSocket m, SocketInfo m)

    connectToServer :: String -> String -> m (CommunicatorSocket m)

    readSocket :: CommunicatorSocket m -> m B.ByteString
    writeSocket :: CommunicatorSocket m -> B.ByteString -> m ()

    closeSocket :: CommunicatorSocket m -> m ()