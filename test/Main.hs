{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}

module Main (main) where

import Control.Applicative (Alternative (..))
import Control.Monad (MonadPlus (..), void, (<=<))
import Control.Monad.Freer.Internal
import Control.Monad.Freer.NonDet
import Control.Monad.Freer.Reader
import Control.Monad.Freer.Tagged
import Control.Monad.Freer.Tele
import Control.Monad.IO.Class (MonadIO (..))
import Data.Proxy (Proxy (..))

main :: IO ()
main =
  ( print
      <=< ( runM
              . dropEff
                ( \case
                    PutStr _ -> ()
                    GetLine -> ""
                )
              . runWith
                pure
                (\(_ :: NonDet a) _ -> pure empty)
              . makeChoiceN 10
          )
  )
    $ do
      x <- a
      tPrint x
      pure x

a :: (Member NonDet r) => Eff r Int
a = pure 0 <|> fmap (+ 1) a
