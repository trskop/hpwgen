{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}
-- |
-- Module:       $HEADER$
-- Description:  Abstraction over random number generation.
-- Copyright:    (c) 2013 Peter Trsko
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  non-portable (CPP, Rank2Types)
--
-- Abstraction over random number generation.
module Main.Random (withGenRand)
    where

import Control.Applicative (Applicative)
import Data.Word (Word32)

#ifdef WITH_MONADRANDOM
import Control.Monad.Random as MonadRandom (evalRandIO, getRandomR)
#elif defined WITH_MWC_RANDOM
import System.Random.MWC as MWC (Variate(uniformR), asGenIO, withSystemRandom)
#endif


withGenRand
    :: (forall m.
        (Applicative m, Functor m, Monad m) => (Word32 -> m Word32) -> m a)
    -> IO a
#ifdef WITH_MONADRANDOM
withGenRand f = MonadRandom.evalRandIO $ f genRandom
  where
    genRandom = MonadRandom.getRandomR . (,) 0
#elif defined WITH_MWC_RANDOM
withGenRand f = MWC.withSystemRandom . MWC.asGenIO $ f . genRandom
  where
    genRandom g upperBound = MWC.uniformR (0, upperBound) g
#endif
