module Actors (
    fileWriter,
    socketWriter
) where

import Actor

import System.IO as IO
import qualified Data.ByteString.Lazy as B
import qualified Network.Socket.ByteString.Lazy as N

fileWriter :: FilePath -> IO (Mailbox IO B.ByteString)
fileWriter filePath =
    do
        handle <- IO.openBinaryFile filePath IO.AppendMode
        IO.hSetBuffering handle IO.NoBuffering
        actorTerminal (B.hPut handle)

socketWriter :: N.Socket -> IO (Mailbox IO B.ByteString)
socketWriter socket = actorTerminal (N.sendAll socket)
