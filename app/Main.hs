module Main where

import Control.Monad (forever)

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

main :: IO ()
main = pure ()
