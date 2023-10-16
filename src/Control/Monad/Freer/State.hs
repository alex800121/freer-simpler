{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Control.Monad.Freer.State
  ( runState,
    get,
    put,
    modify,
    State (..),
  )
where

import Control.Monad.Freer.Internal
import Control.Monad.Freer.Reader
import Control.Monad.Freer.Writer
import Data.Proxy (Proxy (..))

data State s x where
  SGet :: Reader s x -> State s x
  SPut :: !(Writer s x) -> State s x

runState ::
  forall s rs a.
  (Member (State s) (State s ': rs)) =>
  s ->
  Eff (State s ': rs) a ->
  Eff rs (s, a)
runState s (Pure x) = pure (s, x)
runState s (Impure fx g) = case prj (Proxy :: Proxy (State s)) fx of
  Left (SGet Ask) -> runState s (g s)
  Left (SPut (Tell o)) -> runState o (g ())
  Right r -> Impure r (runState s <$> g)

get :: (Member (State s) r) => Eff r s
get = liftFree (SGet Ask)

put :: (Member (State s) r) => s -> Eff r ()
put = liftFree . SPut . Tell

modify :: (Member (State s) r) => (s -> s) -> Eff r ()
modify f = get >>= put . f
