-- import qualified Data.ByteString as B
-- import qualified Network.Socket as N
-- import qualified Network.Socket.ByteString as NB
-- import qualified System.IO as IO
-- import System.Environment (getArgs)

-- import Actor

-- fileWriter :: FilePath -> IO (Mailbox IO B.ByteString)
-- fileWriter filePath = do
--     handle <- IO.openBinaryFile filePath IO.AppendMode
--     IO.hSetBuffering handle IO.NoBuffering
--     registerTerminal (B.hPut handle)

-- connectionIdentifier :: N.SockAddr -> FilePath
-- connectionIdentifier = const "connection.log"

-- socketWriter :: N.Socket -> IO (Mailbox IO B.ByteString)
-- socketWriter socket = registerTerminal (NB.sendAll socket)

-- socketReader :: N.Socket -> [Mailbox IO B.ByteString] -> IO ()
-- socketReader socket = registerSource (NB.recv socket 1024)

-- actorCreator :: String -> String -> IO (Mailbox IO (N.Socket, N.SockAddr))
-- actorCreator host port = registerTerminal $ \(acceptedSocket, clientAddrInfo) -> do
--     let identifier = connectionIdentifier clientAddrInfo
--         hints = N.defaultHints {N.addrSocketType=N.Stream}

--     serverAddrInfo : _ <- N.getAddrInfo (Just hints) (Just host) (Just port)
--     serverSocket <- N.socket (N.addrFamily serverAddrInfo) (N.addrSocketType serverAddrInfo) (N.addrProtocol serverAddrInfo)
--     N.connect serverSocket (N.addrAddress serverAddrInfo)

--     clientWriterMailbox <- socketWriter acceptedSocket
--     clientRequestLoggerMailbox <- fileWriter ("client_" ++ identifier)
--     serverWriterMailbox <- socketWriter serverSocket
--     serverResponseLoggerMailbox <- fileWriter ("server_" ++ identifier)

--     socketReader acceptedSocket [clientRequestLoggerMailbox, serverWriterMailbox]
--     socketReader serverSocket [serverResponseLoggerMailbox, clientWriterMailbox]

-- main :: IO ()
-- main = do
--     [listenPort, host, port] <- getArgs

--     let hints = N.defaultHints {N.addrSocketType=N.Stream, N.addrFlags=[N.AI_PASSIVE]}
--     addrInfo : _ <- N.getAddrInfo (Just hints) (Nothing) (Just listenPort)
--     socket <- N.socket (N.addrFamily addrInfo) (N.addrSocketType addrInfo) (N.addrProtocol addrInfo)
--     N.setSocketOption socket N.ReuseAddr 1
--     N.bind socket (N.addrAddress addrInfo)
--     N.listen socket 5

--     actorCreatorMailbox <- actorCreator host port
--     registerSourceTopLevel (N.accept socket) [actorCreatorMailbox]

main :: IO ()
main = pure ()