{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies #-}

module ActorIO (
    ActorIO,
    runActorIO
) where

import qualified Control.Monad.Except as MTL

import Control.Concurrent (forkIO)
import qualified Control.Concurrent.Chan as C
import Data.Functor (void)
import Data.Time.Clock.POSIX (getPOSIXTime)

import Daemon
import Actor.ActorContext

import Effects

newtype ActorIO a = ActorIO (MTL.ExceptT ActorException IO a)
    deriving (Functor, Applicative, Monad, MTL.MonadError ActorException, MTL.MonadIO)

runActorIO :: ActorIO a -> IO (Either ActorException a)
runActorIO (ActorIO (MTL.ExceptT action)) = action

instance DaemonContext ActorIO where
    daemon = MTL.liftIO . void . forkIO . void . runActorIO

instance ActorContext ActorIO where
    type MailboxBase ActorIO = C.Chan

    newMailboxBase = MTL.liftIO C.newChan

    getMessageBase = MTL.liftIO . C.readChan
    putMessageBase message = MTL.liftIO . flip C.writeChan message

instance TimeContext ActorIO where
    unixTimeStamp = MTL.liftIO $ fmap round getPOSIXTime

instance LoggingContext ActorIO where
    logMessage = MTL.liftIO . putStrLn