module Effects.LoggingContext (
    LoggingContext(..)
) where

class Applicative m => LoggingContext m where
    logMessage :: String -> m ()