{-# LANGUAGE DataKinds, TypeFamilies #-}

module Bracketing where

import qualified Control.Monad.Cont as Cont

bracket :: Monad m => m a -> (a -> m c) -> Cont.ContT b m a
bracket resourceCreator closeAction =
    Cont.ContT $ \callback -> do
        resource <- resourceCreator
        callback resource <* closeAction resource