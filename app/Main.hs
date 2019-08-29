{-# LANGUAGE ConstraintKinds, DataKinds, FlexibleContexts, TypeFamilies #-}

import Control.Monad (when)
import qualified Control.Monad.Except as MTL
import Data.Functor (void)
import qualified Data.ByteString as B

import AllConstraint

import Actor
import ActorIO

import Effects

type ActorContextWith cs m = (ActorContext m, All cs m)
type ActorCreatorContext m = ActorContextWith '[BinFileHandleContext, LoggingContext, SocketContext, TimeContext] m

fileWriter :: ActorContextWith '[BinFileHandleContext] m => FilePath -> m (Mailbox m B.ByteString)
fileWriter filePath = registerTerminal (openBinHandle filePath) (\handle -> writeBinHandle handle) closeBinHandle

connectionIdentifier :: Int -> String -> FilePath
connectionIdentifier timeStamp addrInfo = addrInfo ++ "_" ++ show timeStamp

socketWriter :: ActorContextWith '[SocketContext] m => CommunicatorSocket m -> m (Mailbox m B.ByteString)
socketWriter socket = registerTerminal (pure socket) writeSocket closeSocket

socketReader :: ActorContextWith '[LoggingContext, SocketContext] m => FilePath -> Mailbox m String -> CommunicatorSocket m -> [Mailbox m B.ByteString] -> m ()
socketReader identifier logger socket =
    let recvAction sock = do
            bytes <- readSocket sock
            when (B.length bytes == 0) $ MTL.throwError ActorTreeTerminated
            pure bytes

        cleanupAction sock = do
            closeSocket sock
            putMessage (identifier ++ " disconnected") logger
    in registerSource (pure socket) recvAction cleanupAction

actorCreatorAction :: (ActorCreatorContext m, Show (SocketInfo m)) => Mailbox m String -> String -> String -> (CommunicatorSocket m, SocketInfo m) -> m ()
actorCreatorAction logger host port (acceptedSocket, clientAddrInfo) = do
    timeStamp <- unixTimeStamp
    
    let identifier = connectionIdentifier timeStamp (show clientAddrInfo)
        clientSocketIdentifier = identifier ++ " client socket"
        serverSocketIdentifier = identifier ++ " server socket"
        logFileBase = identifier ++ "_connection.log"

    putMessage (clientSocketIdentifier ++ " connected") logger

    serverSocket <- connectToServer host port
    putMessage (serverSocketIdentifier ++ " connected") logger

    clientWriterMailbox <- socketWriter acceptedSocket
    clientRequestLoggerMailbox <- fileWriter ("client_" ++ logFileBase)
    serverWriterMailbox <- socketWriter serverSocket
    serverResponseLoggerMailbox <- fileWriter ("server_" ++ logFileBase)

    socketReader clientSocketIdentifier logger acceptedSocket [clientRequestLoggerMailbox, serverWriterMailbox]
    socketReader serverSocketIdentifier logger serverSocket [serverResponseLoggerMailbox, clientWriterMailbox]

actorCreator :: (ActorCreatorContext m, Show (SocketInfo m)) => Mailbox m String -> String -> String -> m (Mailbox m (CommunicatorSocket m, SocketInfo m))
actorCreator logger host port = noResource registerTerminal $ actorCreatorAction logger host port

consoleLogger :: ActorContextWith '[LoggingContext] m => m (Mailbox m String)
consoleLogger = noResource registerTerminal logMessage

main :: IO ()
main = void . runActorIO $ do
    args <- getArgs
    case args of
        [listenPort, host, port] -> do
            logger <- consoleLogger
            actorCreatorMailbox <- actorCreator logger host port
            registerTopLevelSource (listenAsServer listenPort) acceptConnection closeSocket [actorCreatorMailbox]
        _ -> logMessage "usage: mitm listenPort host port"

