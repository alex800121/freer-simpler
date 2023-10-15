{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module MyLib (someFunc) where

import Control.Monad ((>=>))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.ST (RealWorld, ST, stToIO)
import Data.Bifunctor (Bifunctor (first))
import Data.Kind (Constraint, Type)
import Data.Monoid (Sum (..))
import Data.Proxy (Proxy (..))
import Data.STRef (modifySTRef, newSTRef, readSTRef)
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

data FFree (fs :: [Type -> Type]) (a :: Type) where
  Pure :: a -> FFree fs a
  Impure :: Union fs x -> (x -> FFree fs a) -> FFree fs a

instance Functor (FFree fs) where
  fmap f (Pure x) = Pure (f x)
  fmap f (Impure x g) = Impure x (fmap f . g)

instance Applicative (FFree fs) where
  f <*> Impure x g = Impure x ((f <*>) . g)
  f <*> Pure x = ($ x) <$> f
  pure = Pure

instance Monad (FFree f) where
  Pure x >>= f = f x
  Impure x g >>= f = Impure x (g >=> f)

data Tele a where
  PutStr :: String -> Tele ()
  GetLine :: Tele String

-- instance (Member m r, MonadIO m) => MonadIO (FFree r) where
--   liftIO = liftFree @m . liftIO

liftFree :: (Member f r) => f a -> FFree r a
liftFree x = Impure (inj x) pure

tPutStr, tPutStrLn :: (Member Tele r) => String -> FFree r ()
tPutStr = liftFree . PutStr
tPutStrLn s = tPutStr s >> tPutStr "\n"

tPrint :: (Member Tele r, Show a) => a -> FFree r ()
tPrint = tPutStrLn . show

tGetLine :: (Member Tele r) => FFree r String
tGetLine = liftFree GetLine

runTele ::
  forall m rs a.
  ( Member Tele (Tele ': rs),
    Member m rs,
    MonadIO m
  ) =>
  FFree (Tele ': rs) a ->
  FFree rs a
runTele (Pure x) = pure x
runTele (Impure fx g) = case prj (Proxy :: Proxy Tele) fx of
  Left (PutStr s) -> liftFree (liftIO @m (putStr s)) >>= runTele @m . g
  Left GetLine -> liftFree (liftIO @m getLine) >>= runTele @m . g
  Right r -> Impure r (runTele @m . g)

data Reader i x where
  Ask :: Reader i i

ask :: (Member (Reader i) r) => FFree r i
ask = liftFree Ask

runReader ::
  forall i rs a.
  (Member (Reader i) (Reader i ': rs)) =>
  i ->
  FFree (Reader i ': rs) a ->
  FFree rs a
runReader _ (Pure x) = pure x
runReader i (Impure fx g) = case prj (Proxy :: Proxy (Reader i)) fx of
  Left Ask -> runReader i (g i)
  Right r -> Impure r (runReader i <$> g)

data Writer o x where
  Tell :: o -> Writer o ()

tell :: (Member (Writer o) r) => o -> FFree r ()
tell = liftFree . Tell

runWriter ::
  forall o rs a.
  (Monoid o, Member (Writer o) (Writer o ': rs)) =>
  FFree (Writer o ': rs) a ->
  FFree rs (o, a)
runWriter (Pure x) = (mempty,) <$> pure x
runWriter (Impure fx g) = case prj (Proxy :: Proxy (Writer o)) fx of
  Left (Tell o) -> first (<> o) <$> runWriter (g ())
  Right r -> Impure r (runWriter <$> g)

data State s x where
  SGet :: Reader s x -> State s x
  SPut :: !(Writer s x) -> State s x

runState ::
  forall s rs a.
  (Member (State s) (State s ': rs)) =>
  s ->
  FFree (State s ': rs) a ->
  FFree rs (s, a)
runState s (Pure x) = pure (s, x)
runState s (Impure fx g) = case prj (Proxy :: Proxy (State s)) fx of
  Left (SGet Ask) -> runState s (g s)
  Left (SPut (Tell o)) -> runState o (g ())
  Right r -> Impure r (runState s <$> g)

get :: (Member (State s) r) => FFree r s
get = liftFree (SGet Ask)

put :: (Member (State s) r) => s -> FFree r ()
put = liftFree . SPut . Tell

modify :: (Member (State s) r) => (s -> s) -> FFree r ()
modify f = get >>= put . f

run :: FFree '[] a -> a
run (Pure x) = x
run _ = error "unreachable"

runM :: forall m a. (Monad m) => FFree '[m] a -> m a
runM (Pure x) = pure x
runM (Impure fx g) = case prj (Proxy @m) fx of
  Left l -> l >>= runM . g
  Right _ -> error "unreachable"

natTransformWith ::
  (Member g r) =>
  (forall x. f x -> g x) ->
  FFree (f ': r) a ->
  FFree r a
natTransformWith _ (Pure x) = pure x
natTransformWith f (Impure fx g) = case prj (Proxy :: Proxy f) fx of
  Left l -> liftFree (f l) >>= natTransformWith f . g
  Right r -> Impure r (natTransformWith f . g)

runSTFree :: (Member IO r) => FFree (ST RealWorld ': r) a -> FFree r a
runSTFree = transformWithS @(ST RealWorld) @IO (const pure) (\_ stx f -> liftFree (stToIO stx) >>= f) ()

transformWithS ::
  forall m0 m1 a r b s.
  (Member m1 r) =>
  (s -> a -> FFree r b) ->
  (forall v. s -> m0 v -> (v -> FFree r b) -> FFree r b) ->
  s ->
  FFree (m0 ': r) a ->
  FFree r b
transformWithS pureF _impureF s (Pure x) = pureF s x
transformWithS pureF impureF s (Impure fx g) = case prj (Proxy :: Proxy m0) fx of
  Left l -> impureF s l (transformWithS @m0 @m1 pureF impureF s . g)
  Right r -> Impure r (transformWithS @m0 @m1 pureF impureF s . g)

type family All (c :: k -> Constraint) (ks :: [k]) :: Constraint where
  All _ '[] = ()
  All c (x ': xs) = (c x, All c xs)

testFunc :: forall s. FFree '[ST s, Tele, IO] Int
testFunc = do
  a <- liftFree @(ST s) $ newSTRef (0 :: Int)
  g a
  where
    g s = do
      tPutStrLn "What's your name?"
      x <- tGetLine
      tPutStrLn ("Hello, " ++ x)
      liftFree $ modifySTRef s (+ 1)
      f s
    f s = do
      tPutStrLn "(r)epeat or (q)uit?"
      x <- tGetLine
      case x of
        "q" -> liftFree $ readSTRef s
        "r" -> g s
        _ -> do
          tPutStrLn "Invalid instruction"
          f s

someFunc :: IO ()
someFunc = do
  s <-
    runM
      . runTele @IO
      . runSTFree
      $ testFunc
  print s
