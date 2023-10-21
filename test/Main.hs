{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MonoLocalBinds #-}

module Main (main) where

import Control.Applicative (Alternative (..))
import Control.Monad (MonadPlus (..))
import Control.Monad.Freer.Internal
import Control.Monad.Freer.NonDet
import Control.Monad.Freer.Reader
import Control.Monad.Freer.Tagged
import Control.Monad.Freer.Tele
import Data.Proxy (Proxy (..))

main :: IO ()
main =
  print
    . take 10
    . runFinalM
    $ liftFree @[] a

a = pure 0 <|> fmap (+ 1) a
