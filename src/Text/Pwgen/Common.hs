-- |
-- Module:       $HEADER$
-- Description:  Commonly used generators of input alphabet.
-- Copyright:    (c) 2013 Peter Trsko
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  portable
--
-- Commonly used generators of input alphabet.
module Text.Pwgen.Common
    (
    -- * Generators
      numbers
    , uppers
    , lowers
    , symbols
    , ambiguous

    -- ** Hexadecimal Numbers
    , lowerHexNumbers
    , upperHexNumbers
    , hexNumbers

    -- ** Vowels and Consonants
    , lowerVowels
    , lowerVowelsY
    , upperVowels
    , upperVowelsY

    , lowerConsonants
    , lowerConsonantsY
    , upperConsonants
    , upperConsonantsY

    -- ** Utility functions
    , construct

    -- * Predicates
    , is

--  , isNumber
--  , isUpper
--  , isLower
--  , isSymbol
--  , isAmbiguous
--  , isHexNumber

    -- ** Vowels and Consonants
    , isLowerVowel
    , isLowerVowelY
    , isUpperVowel
    , isUpperVowelY
    , isVowel
    , isVowelY

--  , isLowerConsonant
--  , isLowerConsonantY
--  , isUpperConsonant
--  , isUpperConsonantY
--  , isConsonant
--  , isConsonantY
    )
    where

import Data.Word (Word8)

import Control.Applicative.Predicate ((<||>))

import Text.Pwgen.FromAscii


-- {{{ Generators -------------------------------------------------------------

-- | Helper function that constructs generator from list of ASCII values.
construct :: FromAscii a => [Word8] -> (a -> t -> t) -> t -> t
construct ws cons = construct' ws
  where
    construct' []     = id
    construct' (x:xs) = cons (fromAscii x) . construct' xs
{-# INLINE construct #-}

-- | Numbers @\'0\', \'1\', ... \'9\'@.
numbers :: FromAscii a => (a -> t -> t) -> t -> t
numbers = construct [48..57]
{-# INLINEABLE numbers #-}

-- | Upper characters @\'A\', \'B\', ... \'Z\'@.
uppers :: FromAscii a => (a -> t -> t) -> t -> t
uppers = construct [65..90]
{-# INLINEABLE uppers #-}

-- | Lower characters @\'a\', \'b\', ... \'z\'@.
lowers :: FromAscii a => (a -> t -> t) -> t -> t
lowers = construct [97..122]
{-# INLINEABLE lowers #-}

-- | ASCII symbols.
symbols :: FromAscii a => (a -> t -> t) -> t -> t
symbols = construct $ concat
    [ [33..47]          -- "!\"#$%&'()*+,-./"
    , [58..64]          -- ":;<=>?@"
    , [133..96]         -- "[\\]^_`"
    , [123..126]        -- "{|}~"
    ]
{-# INLINEABLE symbols #-}

-- | Ambiguous characters, like @8@ and @B@.
ambiguous :: FromAscii a => (a -> t -> t) -> t -> t
ambiguous = construct
    [ 56, 66            -- 8, B
    , 54, 71            -- 6, G
    , 49, 73, 108       -- 1, I, l
    , 48, 68, 79, 81    -- 0, O, Q, D
    , 83, 53            -- S, 5
    , 90, 50            -- Z, 2
    ]
{-# INLINEABLE ambiguous #-}

-- | Hexadecimal numbers @\'0\', \'1\', ... \'9\', \'a\', \'b\', ... \'f\'@
lowerHexNumbers :: FromAscii a => (a -> t -> t) -> t -> t
lowerHexNumbers cons = numbers cons . construct [97..102] cons
{-# INLINEABLE lowerHexNumbers #-}

-- | Hexadecimal numbers @\'0\', \'1\', ... \'9\', \'A\', \'B\', ... \'F\'@
upperHexNumbers :: FromAscii a => (a -> t -> t) -> t -> t
upperHexNumbers cons = numbers cons . construct [65..70] cons
{-# INLINEABLE upperHexNumbers #-}

-- | Hexadecimal numbers @\'0\', \'1\', ... \'9\', \'A\', \'B\', ... \'F\',
-- \'a\', \'b\', ... \'f\'@
hexNumbers :: FromAscii a => (a -> t -> t) -> t -> t
hexNumbers cons = numbers cons . construct [65..70] cons
    . construct [97..102] cons
{-# INLINEABLE hexNumbers #-}

-- | Lower vowels @\'a\', \'e\', \'i\', \'o\', \'u\'@.
lowerVowels :: FromAscii a => (a -> t -> t) -> t -> t
lowerVowels = construct
    [ 97, 101, 105      -- 'a', 'e', 'i'
    , 111, 117          -- 'o', 'u'
    ]
{-# INLINEABLE lowerVowels #-}

-- | Lower vowels including @\'y\'@.
lowerVowelsY :: FromAscii a => (a -> t -> t) -> t -> t
lowerVowelsY cons = lowerVowels cons . cons (fromAscii 121 {- 'y' -})
{-# INLINEABLE lowerVowelsY #-}

-- | Upper vowels @\'A\', \'E\', \'I\', \'O\', \'U\'@.
upperVowels :: FromAscii a => (a -> t -> t) -> t -> t
upperVowels = construct
    [ 65, 69, 73        -- 'A', 'E', 'I'
    , 79, 85            -- 'O', 'U'
    ]
{-# INLINEABLE upperVowels #-}

-- | Upper vowels including @\'Y\'@.
upperVowelsY :: FromAscii a => (a -> t -> t) -> t -> t
upperVowelsY cons = upperVowels cons . cons (fromAscii 89 {- 'Y' -})
{-# INLINEABLE upperVowelsY #-}

lowerConsonants :: FromAscii a => (a -> t -> t) -> t -> t
lowerConsonants = construct
    [ 98, 99, 100       -- 'b', 'c', 'd'
    , 102, 103, 104     -- 'f', 'g', 'h'
    , 106, 107, 108     -- 'j', 'k', 'l'
    , 109, 110, 112     -- 'm', 'n', 'p'
    , 113, 114, 116     -- 'q', 'r', 's'
    , 116, 118, 119     -- 't', 'v', 'w'
    , 122               -- 'z'
    ]
{-# INLINEABLE lowerConsonants #-}

-- | Lower consonants including @\'y\'@.
lowerConsonantsY :: FromAscii a => (a -> t -> t) -> t -> t
lowerConsonantsY cons = lowerConsonants cons . cons (fromAscii 121 {- 'y' -})
{-# INLINEABLE lowerConsonantsY #-}

upperConsonants :: FromAscii a => (a -> t -> t) -> t -> t
upperConsonants = construct
    [ 66, 67, 68        -- 'B', 'C', 'D'
    , 70, 71, 72        -- 'F', 'G', 'H'
    , 74, 75, 76        -- 'J', 'K', 'L'
    , 77, 78, 80        -- 'M', 'N', 'P'
    , 81, 82, 83        -- 'Q', 'R', 'S'
    , 84, 85, 87        -- 'T', 'V', 'W'
    , 90                -- 'Z'
    ]
{-# INLINEABLE upperConsonants #-}

-- | Upper consonants including @\'Y\'@.
upperConsonantsY :: FromAscii a => (a -> t -> t) -> t -> t
upperConsonantsY cons = upperConsonants cons . cons (fromAscii 89 {- 'Y' -})
{-# INLINEABLE upperConsonantsY #-}

-- }}} Generators -------------------------------------------------------------

-- {{{ Predicates -------------------------------------------------------------

is :: (FromAscii a, Eq a) => Word8 -> a -> Bool
is = (==) . fromAscii
{-# INLINE is #-}

-- | Check if argument is one of @\'a\', \'e\', \'i\', \'o\', \'u\'@.
isLowerVowel :: (FromAscii a, Eq a) => a -> Bool
isLowerVowel = is 97 <||> is 101 <||> is 105  <||> is 111 <||> is 117
{-# INLINE isLowerVowel #-}

-- | Check if argument is one of @\'a\', \'e\', \'i\', \'o\', \'u\', \'y\'@.
isLowerVowelY :: (FromAscii a, Eq a) => a -> Bool
isLowerVowelY = isLowerVowel <||> is 121
{-# INLINE isLowerVowelY #-}

-- | Check if argument is one of @\'A\', \'E\', \'I\', \'O\', \'U\'@.
isUpperVowel :: (FromAscii a, Eq a) => a -> Bool
isUpperVowel = is 65 <||> is 69 <||> is 73 <||> is 79 <||> is 85
{-# INLINE isUpperVowel #-}

-- | Check if argument is one of @\'A\', \'E\', \'I\', \'O\', \'U\', \'Y\'@.
isUpperVowelY :: (FromAscii a, Eq a) => a -> Bool
isUpperVowelY = isUpperVowel <||> is 89
{-# INLINE isUpperVowelY #-}

-- | Check if argument is one of @\'a\', \'e\', \'i\', \'o\', \'u\', \'A\',
-- \'E\', \'I\', \'O\', \'U\'@.
isVowel :: (FromAscii a, Eq a) => a -> Bool
isVowel = isLowerVowel <||> isUpperVowel
{-# INLINE isVowel #-}

-- | Check if argument is one of @\'a\', \'e\', \'i\', \'o\', \'u\', \'y\',
-- \'A\', \'E\', \'I\', \'O\', \'U\', \'Y\'@.
isVowelY :: (FromAscii a, Eq a) => a -> Bool
isVowelY = isLowerVowelY <||> isUpperVowelY
{-# INLINE isVowelY #-}

-- }}} Predicates -------------------------------------------------------------
