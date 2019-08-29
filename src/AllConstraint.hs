{-# LANGUAGE DataKinds, PolyKinds, TypeFamilies, TypeOperators #-}

module AllConstraint (
    All
) where

import GHC.Exts

type family All (ks :: [k -> Constraint]) (a :: k) :: Constraint where
    All '[] _ = ()
    All (k ': ks) a = (k a, All ks a)