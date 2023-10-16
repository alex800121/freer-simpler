module Main (main) where

import Control.Monad.Freer
import Control.Monad.Freer.Reader
import Control.Monad.Freer.Tele

main :: IO ()
main = runM . runTele @IO . runReader (123 :: Int) $ do
  i <- ask @Int
  tPrint i
