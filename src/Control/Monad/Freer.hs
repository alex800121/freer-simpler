{-# LANGUAGE DataKinds #-}
module Control.Monad.Freer
  ( Eff (..),
    Member,
    Members,
    liftFree,
    run,
    runM,
    natTransformWith,
  )
where

import Control.Monad.Freer.Internal
-- import Control.Monad.ST (RealWorld, ST, stToIO)


-- runSTFree :: forall r a. (Member IO r) => Eff (ST RealWorld ': r) a -> Eff r a
-- runSTFree = runWithS @(ST RealWorld) (const pure) (\_ stx f -> liftFree (stToIO stx) >>= f) ()


-- testFunc :: forall s. Eff '[ST s, Tele, IO] Int
-- testFunc = do
--   a <- liftFree @(ST s) $ newSTRef (0 :: Int)
--   g a
--   where
--     g s = do
--       tPutStrLn "What's your name?"
--       x <- tGetLine
--       tPutStrLn ("Hello, " ++ x)
--       liftFree $ modifySTRef s (+ 1)
--       f s
--     f s = do
--       tPutStrLn "(r)epeat or (q)uit?"
--       x <- tGetLine
--       case x of
--         "q" -> liftFree $ readSTRef s
--         "r" -> g s
--         _ -> do
--           tPutStrLn "Invalid instruction"
--           f s
--
-- someFunc :: IO ()
-- someFunc = do
--   s <-
--     runM
--       . runTele @IO
--       . runSTFree
--       $ testFunc
--   print s
