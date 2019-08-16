module Actors (
    socketWriterManager,
    serverConnector
) where

import qualified System.IO as IO
import Data.ByteString.Lazy (ByteString, hPut)
import qualified Network.Socket as N
import Network.Socket.ByteString.Lazy (sendAll)

import Actor

fileWriter :: FilePath -> IO (Mailbox IO ByteString)
fileWriter filePath =
    do
        handle <- IO.openBinaryFile filePath IO.AppendMode
        IO.hSetBuffering handle IO.NoBuffering
        actorTerminal (hPut handle)

socketWriter :: N.Socket -> IO (Mailbox IO ByteString)
socketWriter socket = actorTerminal (sendAll socket)

socketWriterManager :: N.Socket -> FilePath -> IO (Mailbox IO ByteString)
socketWriterManager socket logFilePath =
    do
        loggerMailbox <- fileWriter logFilePath
        socketMailbox <- socketWriter socket
        forward [loggerMailbox, socketMailbox]

serverConnector :: Mailbox IO N.Socket -> IO (Mailbox IO (String, String))
serverConnector mailbox =
    actor [mailbox] $ \(host, port) -> do
        let hints = N.defaultHints {N.addrSocketType = N.Stream}
        addr : _ <- N.getAddrInfo (Just hints) (Just host) (Just port)
        socket <- N.socket (N.addrFamily addr) (N.addrSocketType addr) (N.addrProtocol addr)
        N.connect socket (N.addrAddress addr)
        pure socket