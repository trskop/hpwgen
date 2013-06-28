-- |
-- Module:       $HEADER$
-- Description:  Minimalistic lens-like data accessors.
-- Copyright:    (c) 2013 Peter Trsko
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  portable
--
-- Minimalistic lens-like data accessors.
module Main.MiniLens
    ( L
    , get, set, mkL
    , E
    )
    where

import Control.Arrow ((&&&))


type E a = a -> a
type L c a = c -> (a, a -> c)

get :: L c a -> c -> a
get = (fst .)

set :: L c a -> a -> E c
set = flip . (snd .)

mkL :: (c -> a) -> (a -> E c) -> L c a
mkL getter setter = getter &&& flip setter
