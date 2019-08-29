{-# LANGUAGE FlexibleContexts #-}

import Control.Monad (when)
import qualified Control.Monad.Except as MTL
import Data.Functor (void)
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Data.ByteString as B
import qualified Network.Socket as N
import qualified Network.Socket.ByteString as NB
import System.Environment (getArgs)
import qualified System.IO as IO

import Actor
import ActorIO

fileWriter :: FilePath -> ActorIO (Mailbox ActorIO B.ByteString)
fileWriter filePath =
    let initializer = MTL.liftIO $ do
            handle <- IO.openBinaryFile filePath IO.AppendMode
            IO.hSetBuffering handle IO.NoBuffering
            pure handle
    in registerTerminal initializer (\handle -> MTL.liftIO . B.hPut handle) (MTL.liftIO . IO.hClose)

addrInfoToString :: N.SockAddr -> String
addrInfoToString = show

connectionIdentifier :: Int -> N.SockAddr -> FilePath
connectionIdentifier timeStamp addrInfo = addrInfoToString addrInfo ++ "_" ++ show timeStamp

socketWriter :: N.Socket -> ActorIO (Mailbox ActorIO B.ByteString)
socketWriter socket = registerTerminal (pure socket) (\sock -> MTL.liftIO . NB.sendAll sock) (MTL.liftIO . N.close)

socketReader :: FilePath -> Mailbox ActorIO String -> N.Socket -> [Mailbox ActorIO B.ByteString] -> ActorIO ()
socketReader identifier logger socket =
    let recvAction sock = do
            bytes <- MTL.liftIO $ NB.recv sock 1024
            when (B.length bytes == 0) $ MTL.throwError ActorTreeTerminated
            pure bytes

        cleanupAction sock = do
            MTL.liftIO $ N.close sock
            putMessage (identifier ++ " disconnected") logger
    in registerSource (pure socket) recvAction cleanupAction

actorCreatorAction :: Mailbox ActorIO String -> String -> String -> (N.Socket, N.SockAddr) -> ActorIO ()
actorCreatorAction logger host port (acceptedSocket, clientAddrInfo) = do
    timeStamp <- MTL.liftIO $ fmap round getPOSIXTime
    
    let identifier = connectionIdentifier timeStamp clientAddrInfo
        clientSocketIdentifier = identifier ++ " client socket"
        serverSocketIdentifier = identifier ++ " server socket"
        logFileBase = identifier ++ "_connection.log"
        hints = N.defaultHints {N.addrSocketType=N.Stream}

    putMessage (clientSocketIdentifier ++ " connected") logger

    serverSocket <- MTL.liftIO $ do
        serverAddrInfo : _ <- N.getAddrInfo (Just hints) (Just host) (Just port)
        serverSocket <- N.socket (N.addrFamily serverAddrInfo) (N.addrSocketType serverAddrInfo) (N.addrProtocol serverAddrInfo)
        N.connect serverSocket (N.addrAddress serverAddrInfo)
        pure serverSocket

    putMessage (serverSocketIdentifier ++ " connected") logger

    clientWriterMailbox <- socketWriter acceptedSocket
    clientRequestLoggerMailbox <- fileWriter ("client_" ++ logFileBase)
    serverWriterMailbox <- socketWriter serverSocket
    serverResponseLoggerMailbox <- fileWriter ("server_" ++ logFileBase)

    socketReader clientSocketIdentifier logger acceptedSocket [clientRequestLoggerMailbox, serverWriterMailbox]
    socketReader serverSocketIdentifier logger serverSocket [serverResponseLoggerMailbox, clientWriterMailbox]

actorCreator :: Mailbox ActorIO String -> String -> String -> ActorIO (Mailbox ActorIO (N.Socket, N.SockAddr))
actorCreator logger host port = noResource registerTerminal $ actorCreatorAction logger host port

consoleLogger :: ActorIO (Mailbox ActorIO String)
consoleLogger = noResource registerTerminal $ MTL.liftIO . putStrLn

main :: IO ()
main =
    let initializer listenPort = MTL.liftIO $ do
            let hints = N.defaultHints {N.addrSocketType=N.Stream, N.addrFlags=[N.AI_PASSIVE]}
            addrInfo : _ <- N.getAddrInfo (Just hints) (Nothing) (Just listenPort)
            socket <- N.socket (N.addrFamily addrInfo) (N.addrSocketType addrInfo) (N.addrProtocol addrInfo)
            N.setSocketOption socket N.ReuseAddr 1
            N.bind socket (N.addrAddress addrInfo)
            N.listen socket 5
            pure socket
    in void . runActorIO $ do
        args <- MTL.liftIO getArgs
        case args of
            [listenPort, host, port] -> do
                logger <- consoleLogger
                actorCreatorMailbox <- actorCreator logger host port
                registerTopLevelSource (initializer listenPort) (MTL.liftIO . N.accept) (MTL.liftIO . N.close) [actorCreatorMailbox]
            _ -> MTL.liftIO $ putStrLn "usage: mitm listenPort host port"

