{-# LANGUAGE TypeFamilies #-}

module Effects.Communicator where

import qualified Data.ByteString.Lazy as B
import qualified Control.Monad.Cont as Cont

import Bracketing

class Monad m => Communicator m where
    type Connection m :: *

    makeServerConnection :: String -> String -> m (Connection m)
    acceptClientConnection :: String -> m (Connection m)

    closeConnection :: Connection m -> m ()

    writeConnection :: Connection m -> B.ByteString -> m ()
    readConnection :: Connection m -> m B.ByteString

withConnectionAsServer :: Communicator m => String -> Cont.ContT a m (Connection m)
withConnectionAsServer listenPort = bracket (acceptClientConnection listenPort) closeConnection

withConnectionAsClient :: Communicator m => String -> String -> Cont.ContT a m (Connection m)
withConnectionAsClient host port = bracket (makeServerConnection host port) closeConnection
