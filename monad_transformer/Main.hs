import IdentityT
import MaybeT
import ReaderT
import StateT

{-
-- type Maybe a = MaybeT Identity a

-- maybe :: Maybe a
-- maybe = MaybeT { runMaybeT = Identity { runIdentity = (Just 4) }}
-}