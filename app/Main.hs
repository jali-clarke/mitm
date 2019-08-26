import qualified Data.ByteString as B
import qualified Network.Socket as N
import qualified Network.Socket.ByteString as NB
import qualified System.IO as IO
import System.Environment (getArgs)

import Actor

fileWriter :: FilePath -> IO (Mailbox IO B.ByteString)
fileWriter filePath = do
    handle <- IO.openBinaryFile filePath IO.AppendMode
    IO.hSetBuffering handle IO.NoBuffering
    registerTerminal (B.hPut handle)

filePathForWriter :: N.SockAddr -> FilePath
filePathForWriter = undefined

socketWriter :: String -> String -> IO (Mailbox IO B.ByteString)
socketWriter host port = do
    let hints = N.defaultHints {N.addrSocketType=N.Stream}
    addrInfo : _ <- N.getAddrInfo (Just hints) (Just host) (Just port)
    sock <- N.socket (N.addrFamily addrInfo) (N.addrSocketType addrInfo) (N.addrProtocol addrInfo)
    N.connect sock (N.addrAddress addrInfo)
    registerTerminal (NB.sendAll sock)

socketReader :: N.Socket -> [Mailbox IO B.ByteString] -> IO ()
socketReader socket = registerSource (NB.recv socket 1024)

actorCreator :: String -> String -> IO (Mailbox IO (N.Socket, N.SockAddr))
actorCreator host port = registerTerminal $ \(sock, addrInfo) ->
    sequence [socketWriter host port, fileWriter (filePathForWriter addrInfo)] >>= socketReader sock

main :: IO ()
main = do
    [listenPort, host, port] <- getArgs

    let hints = N.defaultHints {N.addrSocketType=N.Stream, N.addrFlags=[N.AI_PASSIVE]}
    addrInfo : _ <- N.getAddrInfo (Just hints) (Nothing) (Just listenPort)
    sock <- N.socket (N.addrFamily addrInfo) (N.addrSocketType addrInfo) (N.addrProtocol addrInfo)
    N.setSocketOption sock N.ReuseAddr 1
    N.bind sock (N.addrAddress addrInfo)
    N.listen sock 5

    sequence [actorCreator host port] >>= registerSourceTopLevel (N.accept sock)