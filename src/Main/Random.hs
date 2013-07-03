{-# LANGUAGE Rank2Types #-}
-- |
-- Module:       $HEADER$
-- Description:  Abstraction over random number generation.
-- Copyright:    (c) 2013 Peter Trsko
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  non-portable (Rank2Types)
--
-- Abstraction over random number generation.
module Main.Random (withGenRand)
    where

import Control.Applicative (Applicative)
import Data.Word (Word32)

import Control.Monad.Random as MonadRandom (evalRandIO, getRandomR)


withGenRand
    :: (forall m.
        (Applicative m, Functor m, Monad m) => (Word32 -> m Word32) -> m a)
    -> IO a
withGenRand f = MonadRandom.evalRandIO $ f genRandom
  where
    genRandom = MonadRandom.getRandomR . (,) 0
