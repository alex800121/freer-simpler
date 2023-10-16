{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Freer
  ( Eff (..),
    Member,
    Members,
    liftFree,
    run,
    runM,
    natTransformWith,
    transformWithS,
  )
where

import Control.Monad.Freer.Internal
import Data.Kind (Constraint)


-- runSTFree :: (Member IO r) => Eff (ST RealWorld ': r) a -> Eff r a
-- runSTFree = transformWithS @(ST RealWorld) @IO (const pure) (\_ stx f -> liftFree (stToIO stx) >>= f) ()

type family All (c :: k -> Constraint) (ks :: [k]) :: Constraint where
  All _ '[] = ()
  All c (x ': xs) = (c x, All c xs)

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
