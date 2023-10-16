{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Freer.Internal
  ( Eff (..),
    Member,
    Members,
    liftFree,
    run,
    runM,
    natTransformWith,
    transformWithS,
    Inj (..),
    Prj (..),
  )
where

import Control.Monad ((>=>))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Kind (Constraint, Type)
import Data.Proxy (Proxy (..))
import GHC.TypeLits (ErrorMessage (..), TypeError)

data Union (rs :: [Type -> Type]) (v :: Type) where
  Here :: r v -> Union (r ': rs) v
  There :: Union rs v -> Union (r ': rs) v

class Inj x xs where
  inj :: x v -> Union xs v

instance {-# OVERLAPPING #-} Inj x (x ': xs) where
  inj = Here

instance (Inj x xs) => Inj x (y ': xs) where
  inj = There . inj

instance (TypeError (Text "Inj error")) => Inj x '[] where
  inj = error "unreachable"

class (PrjF x xs ~ ys) => Prj x xs ys where
  prj :: Proxy x -> Union xs v -> Either (x v) (Union ys v)

type family PrjF x xs where
  PrjF x (x ': xs) = xs
  PrjF x (y ': xs) = y ': PrjF x xs
  PrjF x '[] = TypeError (Text "Prj error")

instance (PrjF x (x ': xs) ~ xs) => Prj x (x ': xs) xs where
  prj _ (Here x) = Left x
  prj _ (There xs) = Right xs

instance
  {-# INCOHERENT #-}
  (PrjF x (y ': xs) ~ (y ': ys), Prj x xs ys) =>
  Prj x (y ': xs) (y ': ys)
  where
  prj _ (Here x) = Right (inj x)
  prj p (There xs) = There <$> prj p xs

instance (PrjF x '[] ~ ys, TypeError (Text "Prj error")) => Prj x '[] ys where
  prj = error "unreachable"

class (Prj x xs (PrjF x xs), Inj x xs) => Member x xs

instance (Prj x xs (PrjF x xs), Inj x xs) => Member x xs

type family Members (xs :: [Type -> Type]) (ys :: [Type -> Type]) :: Constraint where
  Members '[] ys = ()
  Members (x ': xs) ys = (Member x ys, Members xs ys)

data Eff (fs :: [Type -> Type]) (a :: Type) where
  Pure :: a -> Eff fs a
  Impure :: Union fs x -> (x -> Eff fs a) -> Eff fs a

instance Functor (Eff fs) where
  fmap f (Pure x) = Pure (f x)
  fmap f (Impure x g) = Impure x (fmap f . g)

instance Applicative (Eff fs) where
  f <*> Impure x g = Impure x ((f <*>) . g)
  f <*> Pure x = ($ x) <$> f
  pure = Pure

instance Monad (Eff f) where
  Pure x >>= f = f x
  Impure x g >>= f = Impure x (g >=> f)

instance (Member m r, MonadIO m) => MonadIO (Eff r) where
  liftIO = liftFree @m . liftIO

liftFree :: (Member f r) => f a -> Eff r a
liftFree x = Impure (inj x) pure

run :: Eff '[] a -> a
run (Pure x) = x
run _ = error "unreachable"

runM :: forall m a. (Monad m) => Eff '[m] a -> m a
runM (Pure x) = pure x
runM (Impure fx g) = case prj (Proxy @m) fx of
  Left l -> l >>= runM . g
  Right _ -> error "unreachable"

natTransformWith ::
  (Member g r) =>
  (forall x. f x -> g x) ->
  Eff (f ': r) a ->
  Eff r a
natTransformWith _ (Pure x) = pure x
natTransformWith f (Impure fx g) = case prj (Proxy :: Proxy f) fx of
  Left l -> liftFree (f l) >>= natTransformWith f . g
  Right r -> Impure r (natTransformWith f . g)

transformWithS ::
  forall m0 m1 a r b s.
  (Member m1 r) =>
  (s -> a -> Eff r b) ->
  (forall v. s -> m0 v -> (v -> Eff r b) -> Eff r b) ->
  s ->
  Eff (m0 ': r) a ->
  Eff r b
transformWithS pureF _impureF s (Pure x) = pureF s x
transformWithS pureF impureF s (Impure fx g) = case prj (Proxy :: Proxy m0) fx of
  Left l -> impureF s l (transformWithS @m0 @m1 pureF impureF s . g)
  Right r -> Impure r (transformWithS @m0 @m1 pureF impureF s . g)

type family All (c :: k -> Constraint) (ks :: [k]) :: Constraint where
  All _ '[] = ()
  All c (x ': xs) = (c x, All c xs)
