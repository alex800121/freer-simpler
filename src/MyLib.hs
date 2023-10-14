{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module MyLib (someFunc) where

import Control.Monad ((>=>))
import Data.Bifunctor (Bifunctor (first))
import Data.Functor.Identity (Identity (..))
import Data.Kind (Constraint, Type)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Proxy (Proxy (..))
import GHC.TypeLits (ErrorMessage (..), TypeError)

-- data Union (rs :: NonEmpty (Type -> Type)) (v :: Type) where
--   Last :: Union (Identity ':| '[]) v
--   Here :: r v -> Union rs v
--   There :: Union (r ':| rs) v -> Union (r' ':| (r ': rs)) v
-- instance {-# OVERLAPPING #-} Inj x (x ':| xs) where
--   inj = Here
-- instance (Inj x (x' ':| xs)) => Inj x (y ':| (x' ': xs)) where
--   inj = There . inj
-- instance Prj x (x ':| (x' ': xs)) (x' ':| xs) where
--   prj _ (Here x) = Left x
--   prj _ (There xs) = Right xs
-- instance Prj x (x ':| '[]) (Identity ':| '[]) where
--   prj _ (Here x) = Left x
--   prj _ (There xs) = Right xs
-- instance
--   (PrjF x (y ':| (y' ': xs)) ~ Cons y (PrjF x (y' ':| xs))
--   , ys' ~ Cons y (PrjF x (y' ':| xs))
--   , Prj x (y' ':| xs) (y' ':| ys)) => Prj x (y ':| (y' ': xs)) ys' where
--   -- instance (Prj x xs ys) => Prj x (y ': xs) (y ': ys) where
--   prj _ (Here x) = Right (inj x)
--   prj p (There xs) = There <$> prj p xs
-- type family PrjF x xs where
--   PrjF x (x ':| (x' ': xs)) = (x' :| xs)
--   PrjF x (x ':| '[]) = Identity ':| '[]
--   PrjF x (y ':| (y' ': xs)) = Cons y (PrjF x (y' ':| xs))
--   PrjF x (y ':| '[]) = TypeError (Text "Prj error")
-- type family Cons x xs where
--   Cons x (y ':| ys) = x ':| (y ': ys)

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

instance Prj x (x ': xs) xs where
  prj _ (Here x) = Left x
  prj _ (There xs) = Right xs

instance (PrjF x (y ': xs) ~ (y ': ys), Prj x xs ys) => Prj x (y ': xs) (y ': ys) where
  -- instance (Prj x xs ys) => Prj x (y ': xs) (y ': ys) where
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

liftFree :: (Member f r) => f a -> FFree r a
liftFree x = Impure (inj x) pure

tPutStr :: (Member Tele r) => String -> FFree r ()
tPutStr = liftFree . PutStr

tGetLine :: (Member Tele r) => FFree r String
tGetLine = liftFree GetLine

runTele ::
  FFree '[Tele] a ->
  IO a
runTele (Pure x) = pure x
runTele (Impure fx g) = case prj (Proxy :: Proxy Tele) fx of
  Left (PutStr s) -> putStr s >> runTele (g ())
  Left GetLine -> getLine >>= runTele . g
  Right _ -> error "unreachable"

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
runWriter (Pure x) = pure (mempty, x)
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

runM :: (Monad m) => FFree '[m] a -> m a
runM (Pure x) = pure x
runM (Impure fx g) = case prj (Proxy :: Proxy m) fx of
  Left l -> l >>= runM . g
  Right _ -> error "unreachable"

-- testFunc :: (Members '[State Int, Reader Int, Writer String, Tele] r) => FFree r Int
testFunc = do
  x <- ask @Int
  y <- get @Int
  tell $ show (x + y)
  z <- tGetLine
  tPutStr z
  tPutStr "\n"
  modify @Int (+ 10000)
  tell $ "Hello" ++ z
  get @Int

someFunc :: IO ()
someFunc = do
  s <- runTele
        . runWriter @String
        . runState (456 :: Int)
        . runReader (123 :: Int)
        $ testFunc
  print s
