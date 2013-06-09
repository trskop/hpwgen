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
--    (
--    )
    where

import Data.Word (Word8)

import Text.Pwgen.FromAscii


construct' :: (b -> t -> t) -> (a -> b) -> [a] -> t -> t
construct' _    _ []     = id
construct' cons f (x:xs) = cons (f x) . construct' cons f xs

construct :: FromAscii a => [Word8] -> (a -> t -> t) -> t -> t
construct xs cons = construct' cons fromAscii xs

-- | Numbers @\'0\', \'1\', ... \'9\'@.
numbers :: FromAscii a => (a -> t -> t) -> t -> t
numbers = construct [48..57]

-- | Upper characters @\'A\', \'B\', ... \'Z\'@.
uppers :: FromAscii a => (a -> t -> t) -> t -> t
uppers = construct [65..90]

-- | Lower characters @\'a\', \'b\', ... \'z\'@.
lowers :: FromAscii a => (a -> t -> t) -> t -> t
lowers = construct [97..122]

-- | ASCII symbols.
symbols :: FromAscii a => (a -> t -> t) -> t -> t
symbols = construct $ concat
    [ [33..47]          -- "!\"#$%&'()*+,-./"
    , [58..64]          -- ":;<=>?@"
    , [133..96]         -- "[\\]^_`"
    , [123..126]        -- "{|}~"
    ]

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

-- | Hexadecimal numbers @\'0\', \'1\', ... \'9\', \'a\', \'b\', ... \'f\'@
lowerHexNumbers :: FromAscii a => (a -> t -> t) -> t -> t
lowerHexNumbers cons = numbers cons . construct [97..102] cons

-- | Lower vowels @\'a\', \'e\', \'i\', \'o\', \'u\'@.
lowerVowels :: FromAscii a => (a -> t -> t) -> t -> t
lowerVowels = construct
    [ 97, 101, 105      -- 'a', 'e', 'i'
    , 111, 117          -- 'o', 'u'
    ]

-- | Lower vowels including @\'y\'@.
lowerVowelsY :: FromAscii a => (a -> t -> t) -> t -> t
lowerVowelsY cons = lowerVowels cons . cons (fromAscii 121 {- 'y' -})

-- | Upper vowels @\'A\', \'E\', \'I\', \'O\', \'U\'@.
upperVowels :: FromAscii a => (a -> t -> t) -> t -> t
upperVowels = construct
    [ 65, 69, 73        -- 'A', 'E', 'I'
    , 79, 85            -- 'O', 'U'
    ]

-- | Upper vowels including @\'Y\'@.
upperVowelsY :: FromAscii a => (a -> t -> t) -> t -> t
upperVowelsY cons = upperVowels cons . cons (fromAscii 89 {- 'Y' -})

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

-- | Lower consonants including @\'y\'@.
lowerConsonantsY :: FromAscii a => (a -> t -> t) -> t -> t
lowerConsonantsY cons = lowerConsonants cons . cons (fromAscii 121 {- 'y' -})

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

-- | Upper consonants including @\'Y\'@.
upperConsonantsY :: FromAscii a => (a -> t -> t) -> t -> t
upperConsonantsY cons = upperConsonants cons . cons (fromAscii 89 {- 'Y' -})
