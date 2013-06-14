{-# LANGUAGE CPP #-}
-- |
-- Module:       $HEADER$
-- Description:  Class for constructing characters from ASCI values.
-- Copyright:    (c) 2013 Peter Trsko
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    stable
-- Portability:  non-portable (CPP)
--
-- Class for constructing characters from ASCI values. This allows to abstract
-- from actual character implementation and as a consequence it also doesn't
-- dictate any string\/text implementation.
--
-- This module reexports 'FromAscii' class defined in
-- "Text.Pwgen.FromAscii.Class" along with @ByteString@ and @Text@ if compiled
-- with @ByteString@ and @Text@ support.
module Text.Pwgen.FromAscii (FromAscii(..))
    where

import Text.Pwgen.FromAscii.Class (FromAscii(..))
#ifdef MIN_VERSION_bytestring
import Text.Pwgen.FromAscii.ByteString ()
#endif
#ifdef MIN_VERSION_text
import Text.Pwgen.FromAscii.Text ()
#endif
