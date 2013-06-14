{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module:       $HEADER$
-- Description:  FromAscii instances for Text
-- Copyright:    (c) 2013 Peter Trsko
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    stable
-- Portability:  non-portable (CPP)
--
-- 'FromAscii' instances for Text.
module Text.Pwgen.FromAscii.Text (FromAscii(..))
    where

import qualified Data.Text as Text (Text, singleton)
import qualified Data.Text.Lazy as LazyText (Text, singleton)

-- Text Builder, introduced in version 0.8.0.0.
#if MIN_VERSION_text(0, 8, 0)
import qualified Data.Text.Lazy.Builder as TextBuilder (Builder, singleton)
#endif

import Text.Pwgen.FromAscii.Class (FromAscii(..))


instance FromAscii Text.Text where
    fromAscii = Text.singleton . fromAscii
    {-# INLINE fromAscii #-}

instance FromAscii LazyText.Text where
    fromAscii = LazyText.singleton . fromAscii
    {-# INLINE fromAscii #-}

#if MIN_VERSION_text(0, 8, 0)
instance FromAscii TextBuilder.Builder where
    fromAscii = TextBuilder.singleton . fromAscii
    {-# INLINE fromAscii #-}
#endif
