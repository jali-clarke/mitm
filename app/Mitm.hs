{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies #-}

module Mitm (
    Mitm,
    runMitm,
    liftIO
) where

import qualified Control.Concurrent.Chan as Chan
import qualified System.IO as IO
import qualified Data.ByteString.Lazy as Bytes
import qualified Network.Socket as Network
import qualified Network.Socket.ByteString.Lazy as NetworkBytes

import Effects

newtype Mitm a = Mitm (IO a) deriving (Functor, Applicative, Monad)

runMitm :: Mitm a -> IO a
runMitm (Mitm action) = action

liftIO :: IO a -> Mitm a
liftIO action = Mitm action

instance Actor Mitm where
    type Mailbox Mitm = Chan.Chan Bytes.ByteString

    newMailbox = Mitm (Chan.newChan)

    writeMessage chan a = Mitm (Chan.writeChan chan a)
    readMessage chan = Mitm (Chan.readChan chan)

instance Logger Mitm where
    type LogFile Mitm = IO.Handle

    newLogFile path = Mitm (IO.openBinaryFile path IO.AppendMode)
    closeLogFile logFile = Mitm (IO.hClose logFile)

    writeLogFile handle bytes = Mitm (Bytes.hPut handle bytes)

instance Communicator Mitm where
    type Connection Mitm = Network.Socket

    makeServerConnection host port =
        let hints = Network.defaultHints {Network.addrSocketType = Network.Stream}
        in Mitm $ do
            addrInfo : _ <- Network.getAddrInfo (Just hints) (Just host) (Just port)
            socket <- Network.socket (Network.addrFamily addrInfo) (Network.addrSocketType addrInfo) (Network.addrProtocol addrInfo)
            Network.connect socket (Network.addrAddress addrInfo)
            pure socket

    acceptClientConnection port =
        let hints = Network.defaultHints {Network.addrFlags = [Network.AI_PASSIVE], Network.addrSocketType = Network.Stream}
        in Mitm $ do
            addrInfo : _ <- Network.getAddrInfo (Just hints) Nothing (Just port)
            socket <- Network.socket (Network.addrFamily addrInfo) (Network.addrSocketType addrInfo) (Network.addrProtocol addrInfo)
            Network.setSocketOption socket Network.ReuseAddr 1
            Network.setSocketOption socket Network.ReusePort 1
            Network.bind socket (Network.addrAddress addrInfo)
            Network.listen socket 1
            pure socket

    closeConnection socket = Mitm (Network.close socket)

    writeConnection socket bytes = Mitm (NetworkBytes.sendAll socket bytes)
    readConnection socket = Mitm (NetworkBytes.recv socket 1024)
