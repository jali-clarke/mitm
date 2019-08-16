module Sources (
    socketReader,
    connectionAcceptor
) where

import Data.ByteString.Lazy (ByteString)
import Network.Socket (Socket, SockAddr, accept)
import Network.Socket.ByteString.Lazy (recv)

import Actor
import Source

socketReader :: Socket -> [Mailbox IO ByteString] -> IO ()
socketReader socket = source (recv socket 1024)

connectionAcceptor :: Socket -> [Mailbox IO (Socket, SockAddr)] -> IO ()
connectionAcceptor listenSocket = source (accept listenSocket)