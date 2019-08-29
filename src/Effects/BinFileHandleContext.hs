{-# LANGUAGE TypeFamilies #-}

module Effects.BinFileHandleContext (
    BinFileHandleContext(..)
) where

import qualified Data.ByteString as B

class Monad m => BinFileHandleContext m where
    type FileHandle m :: *

    openBinHandle :: FilePath -> m (FileHandle m)
    writeBinHandle :: FileHandle m -> B.ByteString -> m ()
    closeBinHandle :: FileHandle m -> m ()