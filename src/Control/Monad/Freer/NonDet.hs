{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Freer.NonDet
  ( NonDet (..),
    makeChoice,
    makeChoiceA,
    makeChoiceN,
    listChoice,
    MonadLogic (..),
  )
where

import Control.Applicative (Alternative (..))
import Control.Monad.Freer.Internal
import Data.Proxy (Proxy (..))

listChoice ::
  (Member [] r) =>
  Eff (NonDet ': r) a ->
  Eff r a
listChoice =
  natTransformWith
    ( \case
        MZero -> []
        MPlus -> [True, False]
    )

makeChoiceN ::
  (Member NonDet r) =>
  Int ->
  Eff r a ->
  Eff r [a]
makeChoiceN n x
  | n <= 0 = pure []
  | otherwise =
      msplit x >>= \case
        Nothing -> pure []
        Just (y, ys) -> (y :) <$> makeChoiceN (n - 1) ys

makeChoice ::
  (Alternative f) =>
  Eff (NonDet ': r) a ->
  Eff r (f a)
makeChoice = loop []
  where
    loop [] (Pure x) = pure (pure x)
    loop (y : ys) (Pure x) = (pure x <|>) <$> loop ys y
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
