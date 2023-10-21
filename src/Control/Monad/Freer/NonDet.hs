{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Freer.NonDet
  ( NonDet (..),
    makeChoice,
    MonadLogic (..),
  )
where

import Control.Applicative (Alternative (..))
import Control.Monad.Freer.Internal

makeChoice ::
  (Alternative f) =>
  Eff (NonDet ': r) a ->
  Eff r (f a)
makeChoice =
  runWith
    (pure . pure)
    ( \a f -> case a of
        MZero -> pure empty
        MPlus -> (<|>) <$> f True <*> f False
    )
