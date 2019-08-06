{-# LANGUAGE TypeFamilies #-}

module Effects.Logger where

import qualified Data.ByteString.Lazy as B
import qualified Control.Monad.Cont as Cont

import Bracketing

class Monad m => Logger m where
    type LogFile m :: *

    newLogFile :: String -> m (LogFile m)
    writeLogFile :: LogFile m -> B.ByteString -> m ()

    closeLogFile :: LogFile m -> m ()

withLogFile :: Logger m => String -> Cont.ContT a m (LogFile m)
withLogFile path = bracket (newLogFile path) closeLogFile