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
  i ->
  Eff (Reader i ': rs) a ->
  Eff rs a
runReader =
  runWithS
    (const pure)
    ( \i a f -> case a of
        Ask -> f i
    )
