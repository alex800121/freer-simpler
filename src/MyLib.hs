{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module MyLib (someFunc) where

import Control.Monad ((>=>))
import Data.Bifunctor (Bifunctor (first))
import Data.Kind (Type)
import Data.Proxy (Proxy (..))

data FFree f a where
  Pure :: a -> FFree f a
  Impure :: Union f x -> (x -> FFree f a) -> FFree f a

instance Functor (FFree f) where
  fmap f (Pure x) = Pure (f x)
  fmap f (Impure x g) = Impure x (fmap f . g)

instance Applicative (FFree f) where
  f <*> Pure x = ($ x) <$> f
  f <*> Impure x g = Impure x ((f <*>) . g)
  pure = Pure

instance Monad (FFree f) where
  Pure x >>= f = f x
  Impure x g >>= f = Impure x (g >=> f)

data Union (r :: [Type -> Type]) (v :: Type) where
  Here :: x v -> Union (x ': r) v
  There :: Union rs v -> Union (r ': rs) v

class Inj x xs where
  inj :: x v -> Union xs v

instance {-# OVERLAPPING #-} Inj x (x ': xs) where
  inj = Here

instance (Inj x xs) => Inj x (y ': xs) where
  inj = There . inj

class (PrjF x xs ~ ys) => Prj x xs ys where
  prj :: Proxy x -> Union xs v -> Either (x v) (Union ys v)

type family PrjF x xs where
  PrjF x (x ': xs) = xs
  PrjF x (y ': xs) = y ': PrjF x xs

instance Prj x (x ': xs) xs where
  prj _ (Here x) = Left x
  prj _ (There xs) = Right xs

instance (PrjF x (y ': xs) ~ (y ': ys), Prj x xs ys) => Prj x (y ': xs) (y ': ys) where
  prj _ (Here x) = Right (inj x)
  prj p (There xs) = There <$> prj p xs

class (Prj x xs (PrjF x xs), Inj x xs) => Member x xs

instance (Prj x xs (PrjF x xs), Inj x xs) => Member x xs

data Tele a where
  TellStr :: String -> Tele ()
  GetLine :: Tele String

interpretTele :: Tele a -> IO a
interpretTele (TellStr s) = Prelude.putStr s
interpretTele GetLine = Prelude.getLine

liftFree :: (Member f r) => f a -> FFree r a
liftFree x = Impure (inj x) pure

teleTellStr :: (Member Tele r) => String -> FFree r ()
teleTellStr = liftFree . TellStr

teleGetLine :: (Member Tele r) => FFree r String
teleGetLine = liftFree GetLine

data Reader i x where
  Ask :: Reader i i

ask :: (Member (Reader i) r) => FFree r i
ask = liftFree Ask

runReader ::
  forall i rs a.
  (Prj (Reader i) (Reader i ': rs) rs) =>
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
  (Monoid o, Prj (Writer o) (Writer o ': rs) rs) =>
  FFree (Writer o ': rs) a ->
  FFree rs (o, a)
runWriter (Pure x) = pure (mempty, x)
runWriter (Impure fx g) = case prj (Proxy :: Proxy (Writer o)) fx of
  Left (Tell o) -> first (<> o) <$> runWriter (g ())
  Right r -> Impure r (runWriter <$> g)

data State s x where
  SGet :: Reader s x -> State s x
  SPut :: Writer s x -> State s x

runState ::
  forall s rs a.
  (Prj (State s) (State s ': rs) rs) =>
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
run _ = undefined

someFunc :: IO ()
-- someFunc = runFree runTele $ tell =<< get
someFunc = print $ run $ runState (456 :: Int) $ runWriter @String $ runReader (123 :: Int) $ do
  x <- ask @Int
  y <- get @Int
  tell $ show (x + y)
  modify @Int (+ 10000)
  tell "Hello"
