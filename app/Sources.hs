module Sources (
    socketReader,
    connectionAcceptor
) where

import Data.ByteString.Lazy (ByteString)
import Network.Socket (Socket, SockAddr, accept)
import Network.Socket.ByteString.Lazy (recv)

import Actor
import Daemon

socketReader :: Socket -> Mailbox IO ByteString -> IO (Daemon IO)
socketReader socket mailbox = loopDaemon $ recv socket 1024 >>= writeMailbox mailbox

connectionAcceptor :: Socket -> Mailbox IO (Socket, SockAddr) -> IO (Daemon IO)
connectionAcceptor listenSocket mailbox = loopDaemon $ accept listenSocket >>= writeMailbox mailbox