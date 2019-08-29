module Effects.ArgsContext (
    ArgsContext(..)
) where

class Monad m => ArgsContext m where
    getArgs :: m [String]