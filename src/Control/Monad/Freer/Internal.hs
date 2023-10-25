{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Freer.Internal
  ( Eff (..),
    -- MonadLogic (..),
    Member,
    -- Members,
    liftFree,
    run,
    -- runM,
    -- runT,
    -- runWithS,
    -- runWith,
    -- dropEff,
    -- natTransformWith,
    Inj (..),
    Prj (..),
    Union (..),
    -- NonDet (..),
  )
where

import Control.Applicative (Alternative (..))
import Control.Monad (MonadPlus (..), join, (>=>))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Kind (Constraint, Type)
import Data.Proxy (Proxy (..))
import GHC.TypeLits (ErrorMessage (..), TypeError)

data Union (rs :: [(Type -> Type) -> Type -> Type]) (v :: Type) where
  Here :: r (Eff rs) v -> Union (r ': rs) v
  There :: Union rs v -> Union (r ': rs) v

class Inj x xs ys where
  inj :: x (Eff xs) v -> Union ys v

instance {-# OVERLAPPING #-} Inj x xs (x ': xs) where
  inj = Here

instance (Inj x xs ys) => Inj x (y ': xs) (y ': ys) where
  inj = There . inj

instance (xs ~ TypeError (Text "Inj error")) => Inj x xs '[] where
  inj = error "unreachable"

class (PrjF x xs ~ ys) => Prj x xs ys where
  prj :: Proxy x -> Union xs v -> Either (x (Eff ys) v) (Union ys v)

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
  prj p (There xs) = _f (prj p xs)

instance (PrjF x '[] ~ ys, TypeError (Text "Prj error")) => Prj x '[] ys where
  prj = error "unreachable"

class (Prj x xs (PrjF x xs), Inj x (PrjF x xs) xs) => Member x xs

instance (Prj x xs (PrjF x xs), Inj x (PrjF x xs) xs) => Member x xs

-- type family Members (xs :: [Type -> Type]) (ys :: [Type -> Type]) :: Constraint where
--   Members '[] ys = ()
--   Members (x ': xs) ys = (Member x ys, Members xs ys)

data Eff (fs :: [(Type -> Type) -> Type -> Type]) (a :: Type) where
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

-- instance (Member m r, MonadIO m) => MonadIO (Eff r) where
--   liftIO = liftFree @m . liftIO

liftFree :: (Member f r) => f (Eff r) a -> Eff r a
liftFree x = Impure (inj x) pure

run :: Eff '[] a -> a
run (Pure x) = x
run _ = error "unreachable"

-- runT :: forall r m a. (Monad m, Traversable m) => Eff (m ': r) a -> Eff r (m a)
-- runT (Pure x) = pure (pure x)
-- runT (Impure fx g) = case prj (Proxy @m) fx of
--   Left l -> join <$> mapM (runT . g) l
--   Right r -> Impure r (runT . g)

-- runM :: forall m a. (Monad m) => Eff '[m] a -> m a
-- runM (Pure x) = pure x
-- runM (Impure fx g) = case prj (Proxy @m) fx of
--   Left l -> l >>= runM . g
--   Right _ -> error "unreachable"

-- natTransformWith ::
--   (Member g r) =>
--   (forall x. f x -> g x) ->
--   Eff (f ': r) a ->
--   Eff r a
-- natTransformWith _ (Pure x) = pure x
-- natTransformWith f (Impure fx g) = case prj (Proxy :: Proxy f) fx of
--   Left l -> liftFree (f l) >>= natTransformWith f . g
--   Right r -> Impure r (natTransformWith f . g)
--
-- dropEff :: forall f r a. (forall x. f x -> x) -> Eff (f ': r) a -> Eff r a
-- dropEff _ (Pure x) = pure x
-- dropEff f (Impure fx g) = case prj (Proxy @f) fx of
--   Left l -> dropEff f (g (f l))
--   Right r -> Impure r (dropEff f . g)
--
runWith ::
  forall m a b r.
  (a -> Eff r b) ->
  (forall v m'. m m' v -> (v -> Eff r b) -> Eff r b) ->
  -- (forall v. m (Eff (m ': r)) v -> (v -> Eff r b) -> Eff r b) ->
  Eff (m ': r) a ->
  Eff r b
runWith pureF _impureF (Pure x) = pureF x
runWith pureF impureF (Impure fx g) = case prj (Proxy @m) fx of
  Left l -> impureF l (runWith pureF impureF . g)
  Right r -> Impure r _a

--
-- runWithS ::
--   forall m a b r s.
--   (s -> a -> Eff r b) ->
--   (forall v. s -> m v -> (v -> Eff r b) -> Eff r b) ->
--   s ->
--   Eff (m ': r) a ->
--   Eff r b
-- runWithS pureF _impureF s (Pure x) = pureF s x
-- runWithS pureF impureF s (Impure fx g) = case prj (Proxy @m) fx of
--   Left l -> impureF s l (runWithS @m pureF impureF s . g)
--   Right r -> Impure r (runWithS @m pureF impureF s . g)

type family All (c :: k -> Constraint) (ks :: [k]) :: Constraint where
  All _ '[] = ()
  All c (x ': xs) = (c x, All c xs)

-- data NonDet a where
--   MZero :: NonDet a
--   MPlus :: NonDet Bool
--
-- instance (Member NonDet r) => Alternative (Eff r) where
--   empty = mzero
--   (<|>) = mplus
--
-- instance (Member NonDet r) => MonadPlus (Eff r) where
--   mzero = liftFree MZero
--   mplus m0 m1 = liftFree MPlus >>= \x -> if x then m0 else m1
--
-- instance (Member NonDet r) => MonadFail (Eff r) where
--   fail _ = empty
--
-- class (MonadPlus m) => MonadLogic m where
--   msplit :: m a -> m (Maybe (a, m a))
--   interleave :: m a -> m a -> m a
--   m0 `interleave` m1 = msplit m0 >>= maybe m1 (\(x, xs) -> pure x <|> (m1 `interleave` xs))
--   (>>-) :: m a -> (a -> m b) -> m b
--   m0 >>- f = msplit m0 >>= maybe empty (\(x, xs) -> f x `interleave` (xs >>- f))
--   ifte :: m a -> (a -> m b) -> m b -> m b
--   ifte ma f mb = msplit ma >>= maybe mb (\(x, xs) -> f x <|> (xs >>= f))
--
-- instance (Member NonDet r) => MonadLogic (Eff r) where
--   msplit ::
--     (Member NonDet r) =>
--     Eff r a ->
--     Eff r (Maybe (a, Eff r a))
--   msplit (Pure a) = pure (Just (a, empty))
--   msplit (Impure fx g) = case prj (Proxy @NonDet) fx of
--     Left MZero -> pure Nothing
--     Left MPlus -> do
--       trueBranch <- msplit $ g True
--       case trueBranch of
--         Just (x, xs) -> pure $ Just (x, xs <|> g False)
--         Nothing -> msplit $ g False
--     Right _ -> Impure fx (msplit . g)
