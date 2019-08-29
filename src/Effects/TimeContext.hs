module Effects.TimeContext (
    TimeContext(..)
) where

class Monad m => TimeContext m where
    unixTimeStamp :: m Int