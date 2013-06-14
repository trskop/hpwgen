-- |
-- Module:       $HEADER$
-- Description:  Class for constructing characters from ASCI values.
-- Copyright:    (c) 2013 Peter Trsko
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    stable
-- Portability:  portable
--
-- Class for constructing characters from ASCI values. This allows to abstract
-- from actual character implementation and as a consequence it also doesn't
-- dictate any string\/text implementation.
module Text.Pwgen.FromAscii.Class (FromAscii(..))
    where

import Data.Char (chr)
import Data.Word (Word8)


-- | Class of things that can be constructed from ASCI value of characters.
class FromAscii a where
    -- | Construct character/sequence from ASCI value.
    fromAscii :: Word8 -> a

instance FromAscii Char where
    fromAscii = chr . fromIntegral
    {-# INLINE fromAscii #-}

-- | Implemented as identity.
instance FromAscii Word8 where
    fromAscii = id
    {-# INLINE fromAscii #-}

-- | Constructs a singleton instance of list.
instance FromAscii a => FromAscii [a] where
    fromAscii = (: []) . fromAscii
    {-# INLINE fromAscii #-}
