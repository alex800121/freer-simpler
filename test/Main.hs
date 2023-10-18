{-# LANGUAGE DataKinds #-}

module Main (main) where

import Control.Monad.Freer
import Control.Monad.Freer.Reader
import Control.Monad.Freer.Tagged
import Control.Monad.Freer.Tele
import Data.Proxy (Proxy (..))

instance HasTagged "Hello" (Reader Int)
instance HasTagged "World" (Reader String)

main :: IO ()
main =
  runM
    . runTele @IO
    . runReader (456 :: Int)
    . runTagged (Proxy @"Hello") (runReader 123)
    . runTagged (Proxy @"World") (runReader "World")
    $ do
      i <- tagged @"Hello" $ ask @Int
      tPrint i
      a <- tagged @"World" $ ask @String
      tPutStrLn a
      j <- ask @Int
      tPrint j
      k <- tagged @"Hello" $ ask @Int
      tPrint k
      l <- ask @Int
      tPrint l
