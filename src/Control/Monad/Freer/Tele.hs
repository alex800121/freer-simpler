{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Control.Monad.Freer.Tele
  ( runTele,
    Tele (..),
    tPutStr,
    tPutStrLn,
    tPrint,
    tGetLine,
  )
where

import Control.Monad.Freer.Internal
import Data.Proxy (Proxy (..))
import Control.Monad.IO.Class (MonadIO (liftIO))

data Tele a where
  PutStr :: String -> Tele ()
  GetLine :: Tele String

tPutStr, tPutStrLn :: (Member Tele r) => String -> Eff r ()
tPutStr = liftFree . PutStr
tPutStrLn s = tPutStr s >> tPutStr "\n"

tPrint :: (Member Tele r, Show a) => a -> Eff r ()
tPrint = tPutStrLn . show

tGetLine :: (Member Tele r) => Eff r String
tGetLine = liftFree GetLine

runTele ::
  forall m rs a.
  ( Member Tele (Tele ': rs),
    Member m rs,
    MonadIO m
  ) =>
  Eff (Tele ': rs) a ->
  Eff rs a
runTele (Pure x) = pure x
runTele (Impure fx g) = case prj (Proxy :: Proxy Tele) fx of
  Left (PutStr s) -> liftFree (liftIO @m (putStr s)) >>= runTele @m . g
  Left GetLine -> liftFree (liftIO @m getLine) >>= runTele @m . g
  Right r -> Impure r (runTele @m . g)
