{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module:       $HEADER$
-- Description:  FromAscii instances for ByteStrings
-- Copyright:    (c) 2013 Peter Trsko
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    stable
-- Portability:  non-portable (CPP)
--
-- FromAscii instances for ByteStrings.
module Text.Pwgen.FromAscii.ByteString (FromAscii(..))
    where

import qualified Data.ByteString as BS (ByteString, singleton)
import qualified Data.ByteString.Lazy as LBS (ByteString, singleton)

-- ByteString Builder, interoduced in version 0.10.0.0, changed module name in
-- version 0.10.2.0.
#if MIN_VERSION_bytestring(0, 10, 0)
#if MIN_VERSION_bytestring(0, 10, 2)
import qualified Data.ByteString.Builder as BS (Builder, word8)
#else
import qualified Data.ByteString.Lazy.Builder as BS (Builder, word8)
#endif
#endif

import Text.Pwgen.FromAscii.Class (FromAscii(..))


instance FromAscii BS.ByteString where
    fromAscii = BS.singleton
    {-# INLINE fromAscii #-}

instance FromAscii LBS.ByteString where
    fromAscii = LBS.singleton
    {-# INLINE fromAscii #-}

#if MIN_VERSION_bytestring(0, 10, 0)
instance FromAscii BS.Builder where
    fromAscii = BS.word8
    {-# INLINE fromAscii #-}
#endif
