{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Control.Monad.Freer.Writer
  ( runWriter,
    tell,
    Writer (..),
  )
where

import Control.Monad.Freer.Internal
import Data.Proxy (Proxy (..))
import Data.Bifunctor (first)

data Writer o x where
  Tell :: o -> Writer o ()

tell :: (Member (Writer o) r) => o -> Eff r ()
tell = liftFree . Tell

runWriter ::
  forall o rs a.
  (Monoid o, Member (Writer o) (Writer o ': rs)) =>
  Eff (Writer o ': rs) a ->
  Eff rs (o, a)
runWriter (Pure x) = (mempty,) <$> pure x
runWriter (Impure fx g) = case prj (Proxy :: Proxy (Writer o)) fx of
  Left (Tell o) -> first (<> o) <$> runWriter (g ())
  Right r -> Impure r (runWriter <$> g)
