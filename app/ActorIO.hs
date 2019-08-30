{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies #-}

module ActorIO (
    ActorIO,
    runActorIO
) where

import qualified Control.Monad.Except as MTL

import Control.Concurrent (forkIO)
import qualified Control.Concurrent.Chan as C
import Control.Exception (IOException, catch)
import qualified Data.ByteString as B
import Data.Functor (void)
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Network.Socket as N
import qualified Network.Socket.ByteString as NB
import qualified System.Environment as E
import qualified System.IO as IO

import Daemon
import Actor.ActorContext

import Effects

newtype ActorIO a = ActorIO (MTL.ExceptT ActorException IO a)
    deriving (Functor, Applicative, Monad, MTL.MonadError ActorException, MTL.MonadIO)

runActorIO :: ActorIO a -> IO (Either ActorException a)
runActorIO (ActorIO (MTL.ExceptT action)) = action

instance DaemonContext ActorIO where
    daemon = MTL.liftIO . void . forkIO . void . runActorIO

instance ActorContext ActorIO where
    type MailboxBase ActorIO = C.Chan

    newMailboxBase = MTL.liftIO C.newChan

    getMessageBase = MTL.liftIO . C.readChan
    putMessageBase message = MTL.liftIO . flip C.writeChan message

instance ArgsContext ActorIO where
    getArgs = MTL.liftIO E.getArgs

instance BinFileHandleContext ActorIO where
    type FileHandle ActorIO = IO.Handle

    openBinHandle filePath = MTL.liftIO $ do
        handle <- IO.openBinaryFile filePath IO.AppendMode
        IO.hSetBuffering handle IO.NoBuffering
        pure handle

    writeBinHandle handle bytes = MTL.liftIO $ B.hPut handle bytes

    closeBinHandle handle = MTL.liftIO $ IO.hClose handle

instance LoggingContext ActorIO where
    logMessage = MTL.liftIO . putStrLn

instance SocketContext ActorIO where
    type CommunicatorSocket ActorIO = N.Socket
    type ListenSocket ActorIO = N.Socket
    type SocketInfo ActorIO = N.SockAddr

    acceptConnection listenSocket = MTL.liftIO $ N.accept listenSocket

    listenAsServer listenPort = 
        let hints = N.defaultHints {N.addrSocketType=N.Stream, N.addrFlags=[N.AI_PASSIVE]}
        in MTL.liftIO $ do
            addrInfo : _ <- N.getAddrInfo (Just hints) (Nothing) (Just listenPort)
            socket <- N.socket (N.addrFamily addrInfo) (N.addrSocketType addrInfo) (N.addrProtocol addrInfo)
            N.setSocketOption socket N.ReuseAddr 1
            N.bind socket (N.addrAddress addrInfo)
            N.listen socket 5
            pure socket

    connectToServer host port =
        let hints = N.defaultHints {N.addrSocketType=N.Stream}
        in MTL.liftIO $ do
            serverAddrInfo : _ <- N.getAddrInfo (Just hints) (Just host) (Just port)
            serverSocket <- N.socket (N.addrFamily serverAddrInfo) (N.addrSocketType serverAddrInfo) (N.addrProtocol serverAddrInfo)
            N.connect serverSocket (N.addrAddress serverAddrInfo)
            pure serverSocket

    readSocket socket =
        let handler :: IOException -> IO B.ByteString
            handler = const $ pure B.empty
        in MTL.liftIO $ NB.recv socket 1024 `catch` handler

    writeSocket socket bytes = MTL.liftIO $ NB.sendAll socket bytes

    closeSocket socket = MTL.liftIO $ N.close socket

instance TimeContext ActorIO where
    unixTimeStamp = MTL.liftIO $ fmap round getPOSIXTime