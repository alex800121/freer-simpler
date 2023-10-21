{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}

module Control.Monad.Freer.Tagged
  ( tagged,
    runTagged,
  )
where

import Control.Monad.Freer.Internal
import Data.Kind (Type)
import Data.Proxy (Proxy (..))

data Tagged (tag :: k) (f :: Type -> Type) (x :: Type) where
  Tagged :: f x -> Tagged k f x

tagged ::
  forall k f r a.
  (Member (Tagged k f) r) =>
  Eff (f ': r) a ->
  Eff r a
tagged = natTransformWith (Tagged :: f x -> Tagged k f x)

untagged ::
  forall k f r a.
  Eff (Tagged k f ': r) a ->
  Eff (f ': r) a
untagged (Pure x) = pure x
untagged (Impure fx g) = case prj (Proxy :: Proxy (Tagged k f)) fx of
  Left (Tagged x) -> Impure (Here x) (untagged . g)
  Right r -> Impure (There r) (untagged . g)

runTagged ::
  forall k a b r f.
  (forall r0. Eff (f ': r0) a -> Eff r0 b) ->
  Eff (Tagged k f ': r) a ->
  Eff r b
runTagged f (Pure x) = f (pure x)
runTagged f (Impure fx g) = case prj (Proxy :: Proxy (Tagged k f)) fx of
  Left (Tagged x) -> f (Impure (Here x) pure >>= untagged . g)
  Right r -> Impure r (runTagged f . g)
