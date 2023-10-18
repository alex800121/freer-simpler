{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

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
runTele =
  runWithS
    (const pure)
    ( \_ a f -> case a of
        PutStr s -> liftFree (liftIO @m (putStr s)) >>= f
        GetLine -> liftFree (liftIO @m getLine) >>= f
    )
    ()
