module Main where

import Control.Monad (forever)
import qualified Control.Monad.Cont as Cont
import System.Environment (getArgs)

import Effects
import Mitm

receiveMessagesAndNotify :: Connection Mitm -> Mailbox Mitm -> LogFile Mitm -> Mitm ()
receiveMessagesAndNotify connection targetMailbox targetLogFile =
    forever $ do
        bytes <- readConnection connection
        writeLogFile targetLogFile bytes
        writeMessage targetMailbox bytes

sendMessages :: Connection Mitm -> Mailbox Mitm -> Mitm ()
sendMessages connection sourceMailbox =
    forever $ do
        message <- readMessage sourceMailbox
        writeConnection connection message

spawnAction :: ((Connection Mitm, Mailbox Mitm, LogFile Mitm), (Connection Mitm, Mailbox Mitm, LogFile Mitm)) -> Mitm ()
spawnAction (clientResources, serverResources) =
    pure ()

withResources :: (String, String) -> (String, String, String) -> Cont.ContT a Mitm ((Connection Mitm, Mailbox Mitm, LogFile Mitm), (Connection Mitm, Mailbox Mitm, LogFile Mitm))
withResources (listenPort, clientLogFilePath) (hostAddress, hostPort, serverLogFilePath) =
    do
        socketToClient <- withConnectionAsServer listenPort
        clientResponseMailbox <- Cont.lift newMailbox
        clientLogFile <- withLogFile clientLogFilePath

        socketToServer <- withConnectionAsClient hostAddress hostPort
        serverRequestMailbox <- Cont.lift newMailbox
        serverLogFile <- withLogFile serverLogFilePath

        let clientResources = (socketToClient, clientResponseMailbox, clientLogFile)
        let serverResources = (socketToServer, serverRequestMailbox, serverLogFile)

        pure (clientResources, serverResources)

main :: IO ()
main = do
    [listenPort, host, port] <- getArgs
    let clientResourceNames = (listenPort, "client.log")
    let serverResourceNames = (host, port, "server.log")

    runMitm $ Cont.runContT (withResources clientResourceNames serverResourceNames) spawnAction