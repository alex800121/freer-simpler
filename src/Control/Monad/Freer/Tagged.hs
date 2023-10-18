{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}

module Control.Monad.Freer.Tagged
  ( tagged,
    runTagged,
    HasTagged,
  )
where

import Control.Monad.Freer.Internal
import Data.Kind (Type)
import Data.Proxy (Proxy (..))

class HasTagged (tag :: k) (f :: l) | tag -> f

data Tagged (tag :: k) (f :: Type -> Type) (x :: Type) where
  Tagged :: (HasTagged k f) => {untag :: f x} -> Tagged k f x

tagged ::
  forall k f r a.
  (Member (Tagged k f) r, HasTagged k f) =>
  Eff (f ': r) a ->
  Eff r a
tagged = natTransformWith (Tagged :: f x -> Tagged k f x)

untagged ::
  forall k f r a.
  (HasTagged k f) =>
  Eff (Tagged k f ': r) a ->
  Eff (f ': r) a
untagged (Pure x) = pure x
untagged (Impure fx g) = case prj (Proxy :: Proxy (Tagged k f)) fx of
  Left (Tagged x) -> Impure (Here x) (untagged . g)
  Right r -> Impure (There r) (untagged . g)

runTagged ::
  forall k a b r f.
  (HasTagged k f) =>
  Proxy k ->
  (forall r0. Eff (f ': r0) a -> Eff r0 b) ->
  Eff (Tagged k f ': r) a ->
  Eff r b
runTagged _ f (Pure x) = f (pure x)
runTagged p f (Impure fx g) = case prj (Proxy :: Proxy (Tagged k f)) fx of
  Left (Tagged x) -> f (liftFree x >>= untagged . g)
  Right r -> Impure r (runTagged p f . g)
