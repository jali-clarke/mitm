module Sources (
    socketReader
) where
    
import Control.Monad (forever)
import Data.ByteString.Lazy (ByteString)
import Network.Socket (Socket)
import Network.Socket.ByteString.Lazy (recv)

import Actor
import Daemon

socketReader :: Socket -> Mailbox IO ByteString -> IO (Daemon IO)
socketReader socket mailbox =
    let loop = forever $ recv socket 1 >>= writeMailbox mailbox
    in daemon loop