{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Control.Monad.Freer.Reader
  ( runReader,
    ask,
    Reader (..),
  )
where

import Control.Monad.Freer.Internal

data Reader i x where
  Ask :: Reader i i

ask :: (Member (Reader i) r) => Eff r i
ask = liftFree Ask

runReader ::
  forall i rs a.
  (Member (Reader i) (Reader i ': rs)) =>
  i ->
  Eff (Reader i ': rs) a ->
  Eff rs a
runReader =
  runWithS
    (const pure)
    (\ i a f -> case a of
        Ask -> f i
    )
-- runReader _ (Pure x) = pure x
-- runReader i (Impure fx g) = case prj (Proxy :: Proxy (Reader i)) fx of
--   Left Ask -> runReader i (g i)
--   Right r -> Impure r (runReader i <$> g)
