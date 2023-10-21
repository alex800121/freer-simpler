{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Freer.NonDet
  ( NonDet (..),
    makeChoice,
    makeChoiceA,
    MonadLogic (..),
  )
where

import Control.Applicative (Alternative (..))
import Control.Monad.Freer.Internal
import Data.Proxy (Proxy (..))

makeChoice ::
  (Alternative f) =>
  Eff (NonDet ': r) a ->
  Eff r (f a)
makeChoice = loop []
  where
    loop [] (Pure x) = pure (pure x)
    loop (y : ys) (Pure x) = loop ys y >>= \z -> pure (pure x <|> z)
    loop xs (Impure fx g) = case prj (Proxy @NonDet) fx of
      Left MZero -> case xs of
        [] -> pure empty
        y : ys -> loop ys y
      Left MPlus -> loop (g False : xs) (g True)
      Right r -> Impure r (loop xs . g)

makeChoiceA ::
  (Alternative f) =>
  Eff (NonDet ': r) a ->
  Eff r (f a)
makeChoiceA =
  runWith
    (pure . pure)
    ( \x f -> case x of
        MZero -> pure empty
        MPlus -> (<|>) <$> f True <*> f False
    )
