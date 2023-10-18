{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Control.Monad.Freer.Writer
  ( runWriter,
    tell,
    Writer (..),
  )
where

import Control.Monad.Freer.Internal
import Data.Bifunctor (first)

data Writer o x where
  Tell :: o -> Writer o ()

tell :: (Member (Writer o) r) => o -> Eff r ()
tell = liftFree . Tell

runWriter ::
  (Monoid o) =>
  Eff (Writer o ': rs) a ->
  Eff rs (o, a)
runWriter =
  runWithS
    (\_ a -> pure (mempty, a))
    ( \_ a f -> case a of
        Tell o -> first (<> o) <$> f ()
    )
    ()
