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
import Data.Bifunctor (first)

data State s x where
  Get :: State s s
  Put :: !s -> State s ()

runState ::
  s ->
  Eff (State s ': rs) a ->
  Eff rs (s, a)
runState =
  runWithS
    (curry pure)
    ( \s a f -> case a of
        Get -> f s
        Put o -> first (const o) <$> f ()
    )

get :: (Member (State s) r) => Eff r s
get = liftFree Get

put :: (Member (State s) r) => s -> Eff r ()
put = liftFree . Put

modify :: (Member (State s) r) => (s -> s) -> Eff r ()
modify f = get >>= put . f
