{-# LANGUAGE DataKinds #-}
module Main (main) where

import Control.Applicative (Alternative (..))
import Control.Monad (MonadPlus (..))
import Control.Monad.Freer
import Control.Monad.Freer.NonDet
import Control.Monad.Freer.Reader
import Control.Monad.Freer.Tagged
import Control.Monad.Freer.Tele
import Data.Proxy (Proxy (..))

main :: IO ()
main =
  print
    . run
    . runTagged @"World" (runReader @Int 12)
    . runTagged @"Hello" (runReader @Int 11)
    . runReader @Int 0
    $ do
      i0 <- tagged @"Hello" @(Reader Int) $ ask @Int
      i2 <- ask @Int
      i1 <- tagged @"World" @(Reader Int) $ ask @Int
      return (i0, i1, i2)
