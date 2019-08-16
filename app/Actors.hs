module Actors (
    fileWriter,
    socketWriter
) where

import qualified System.IO as IO
import Data.ByteString.Lazy (ByteString, hPut)
import Network.Socket (Socket)
import Network.Socket.ByteString.Lazy (sendAll)

import Actor

fileWriter :: FilePath -> IO (Mailbox IO ByteString)
fileWriter filePath =
    do
        handle <- IO.openBinaryFile filePath IO.AppendMode
        IO.hSetBuffering handle IO.NoBuffering
        actorTerminal (hPut handle)

socketWriter :: Socket -> IO (Mailbox IO ByteString)
socketWriter socket = actorTerminal (sendAll socket)
