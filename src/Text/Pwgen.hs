{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}
-- |
-- Module:       $HEADER$
-- Description:  Generic code for generating passwords
-- Copyright:    (c) 2013 Peter Trsko
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  non-portable (CPP, Rank2Types)
--
-- Generic code for generating passwords. It doesn't rely on specific random
-- number generator or specific string\/text implementation.
--
-- Function 'genPassword' provides the plumbing, but the core of the algorithm
-- is abstracted in to 'GenPasswordConfig' data type.
module Text.Pwgen
    (
    -- * Generate passwords
      genPassword

    -- ** Configuration
    , GenPasswordConfig(..)

    -- ** Configuration builders
    , mkConfig
    , simpleConfig
    , alternatingConfig
    , alternatingConfigThreeState

    -- * Input symbols/alphabet

    -- ** FromAscii class
    --
    -- | Abstraction for constructing characters/strings without tying your
    -- self to particular character/string implementation.
    , FromAscii(..)

    -- ** Commonly used generators
    , module Text.Pwgen.Common
    )
    where

import Control.Applicative (Applicative, (<$>))
import Data.Word (Word32)

import Text.Pwgen.FromAscii
import Text.Pwgen.Common


-- | Configuration for 'genPassword' function, it actually encodes core
-- parts of the algorithm, while 'genPassword' provides the plubing.
data GenPasswordConfig s t u a = GenPasswordConfig
    { genPwInIndex :: t -> (s, Word32) -> (a, Word32)
    -- ^ Selection function, it takes input alphabet, current state, randomly
    -- generated index and produces pair of selected value and its length.
    , genPwInMaxIndex :: s -> t -> Word32
    -- ^ Get upper bound on index value for input alphabet. Indexing starts
    -- from 0 (zero).
    , genPwIn :: t
    -- ^ Alphabet\/input that characters\/substrings are selected from.
    , genPwStateTransformation
        :: (Applicative m, Functor m, Monad m)
        => (Word32 -> m Word32) -> s -> t -> (a, Word32) -> (u, Word32)
        -> m (Maybe s)
    -- ^ State transformation occurs right after new random index was generated
    -- and element from input alphabet was selected, but before this element is
    -- concanetaed to previously generated sequence.
    --
    -- If it returns 'Nothing' then currently generated element is discarded
    -- and new index is requested from random number generator. This allows to
    -- reject some values based on local invariants, like that vowel has to
    -- follow consonant, etc.
    , genPwInitialState :: s
    -- ^ Initial value of state.
    , genPwOutEmpty :: u
    -- ^ Empty output value.
    , genPwOutCons :: a -> u -> u
    -- ^ Cons function that puts new randomly selected value in to output.
    , genPwOutCond :: s -> u -> Bool
    -- ^ Output condition, if 'False' then whole generated password is
    -- discarded and new value is generated. If set to @\\ _ _ -> 'False'@
    -- then 'genPassword' ends up in infinite loop.
    }

-- | Simplified construction of 'GenPasswordConfig'.
--
-- > mkConfig initialState (input, inLen) (!) e cons = GenPasswordConfig
-- >     { genPwInIndex = (!)
-- >     , genPwInMaxIndex = \ _ _ -> inLen - 1
-- >     , genPwIn = input
-- >     , genPwStateTransformation = \ _ s _ _ _ -> return $ Just s
-- >     , genPwInitialState = initialState
-- >     , genPwOutEmpty = e
-- >     , genPwOutCons = cons
-- >     , genPwOutCond = \ _ _ -> True
-- >     }
mkConfig
    :: s
    -- ^ Initial value of state.
    -> (t, Word32)
    -- ^ Alphabet/input that characters/substrings are selected from.
    -> (t -> (s, Word32) -> (a, Word32))
    -- ^ Selection function, it takes input alphabet, current state, randomly
    -- generated index and produces pair of selected value and its length.
    -> u
    -- ^ Empty output value.
    -> (a -> u -> u)
    -- ^ Cons function that puts new randomly selected value in to output.
    -> GenPasswordConfig s t u a
mkConfig initialState (input, inLen) (!) e cons = GenPasswordConfig
    { genPwInIndex = (!)
    , genPwInMaxIndex = \ _ _ -> inLen - 1 -- Indexing starts from zero.
    , genPwIn = input
    , genPwStateTransformation = \ _ s _ _ _ -> return $ Just s
    , genPwInitialState = initialState
    , genPwOutEmpty = e
    , genPwOutCons = cons
    , genPwOutCond = \ _ _ -> True -- Any sequence is accepted.
    }

-- | Construct simple 'String' based configuration.
simpleConfig
    :: (String, Word32)
    -- ^ Input alphabet and its length.
    -> GenPasswordConfig () String String Char
simpleConfig input = mkConfig
    ()      -- Initial state.
    input   -- Input alphabet and its length.
    (!)     -- Selection function from the input alphabet.
    []      -- Empty output.
    (:)     -- Cons for output.
  where
    l ! (_, n) = (l !! fromIntegral n, 1)

-- | Simplified construction of configuration for situations where there are
-- two input alphabets and algoritm is alternating between these. In other
-- words it uses boolean state that that starts with the first list, then
-- switches to the other and back again in the next round.
alternatingConfig
    :: (t, Word32)
    -> (t, Word32)
    -> (t -> Word32 -> (a, Word32))
    -> u
    -> (a -> u -> u)
    -> GenPasswordConfig Bool (t, t) u a
alternatingConfig (input1, len1) (input2, len2) (!) e cons =
    GenPasswordConfig
        { genPwInIndex = \ (t1, t2) (s, n) -> (! n) $ if s then t2 else t1
        , genPwInMaxIndex = \ s _ -> (if s then len2 else len1) - 1
        , genPwIn = (input1, input2)
        , genPwStateTransformation = \ _ s _ _ _ -> return . Just $ not s
        , genPwInitialState = False
        , genPwOutEmpty = e
        , genPwOutCons = cons
        , genPwOutCond = \ _ _ -> True
        }

-- | Similar to 'alternatingConfig' except that it uses @'Maybe' 'Bool'@ as a
-- state and 'Nothing' is used its initial value. When state is 'Nothing', then
-- element is selected from both input alphabets. If element from first
-- alphabet was selected then state is changed so the second one would be used
-- in the next round. Analogically for the situation if the first element would
-- be from the second alphabet. State doesn't return back to 'Nothing'.
alternatingConfigThreeState
    :: (t, Word32)
    -> (t, Word32)
    -> (t -> t -> t)
    -> (a -> t -> Bool)
    -> (t -> Word32 -> (a, Word32))
    -> u
    -> (a -> u -> u)
    -> GenPasswordConfig (Maybe Bool) (t, t) u a
alternatingConfigThreeState (in1, len1) (in2, len2) (<>) elem' (!) e cons =
    GenPasswordConfig
        { genPwInIndex = \ (t1, t2) (s, n) -> (! n) $ case s of
            Nothing -> t1 <> t2
            Just c
              | c         -> t1
              | otherwise -> t2
        , genPwInMaxIndex = \ s _ -> case s of
            Nothing -> len1 + len2 - 1
            Just c
              | c         -> len1 - 1
              | otherwise -> len2 - 1 , genPwIn = (in1, in2)
        , genPwStateTransformation = \ _ s (t1, t2) (x, _) _ ->
            return $ case s of
                Nothing -> case (x `elem'` t1, x `elem'` t2) of
                    (True, False) -> Just (Just False)
                    (False, True) -> Just (Just True)
                    _ -> Just Nothing
                Just b -> Just (Just (not b))
        , genPwInitialState = Nothing
        , genPwOutEmpty = e
        , genPwOutCons = cons
        , genPwOutCond = \ _ _ -> True
        }

-- | Generate random password based on algorithm described in configuration
-- and using specified random number generator.
genPassword
    :: (Applicative m, Functor m, Monad m, Show a)
    => GenPasswordConfig s t u a
    -- ^ Configuration that describes details of password generation algorithm.
    -> (Word32 -> m Word32)
    -- ^ Generate random value between 0 (zero) and specified upper bound
    -- (including).
    -> Word32
    -- ^ Length of password to be generated.
    -> m u
genPassword cfg genRand pwLength
  | pwLength == 0 = return empty
  | otherwise     = do
    r <- genPassword' (state, empty, 0)
    case r of
        Just (s, pw) | pwCorrect s pw -> return pw
        _ -> genPassword cfg genRand pwLength
            -- Generated password was invalid, start all over again.
  where
    (!) = genPwInIndex cfg
    empty = genPwOutEmpty cfg
    cons = genPwOutCons cfg
    input = genPwIn cfg
    pwCorrect = genPwOutCond cfg
    state = genPwInitialState cfg
    stateTrans = genPwStateTransformation cfg genRand
    max' = flip (genPwInMaxIndex cfg) input

    genPassword' (s, xs, len) = do
        y@(x, n) <- ((input !) . (,) s) <$> genRand (max' s)
        maybeNewState <- stateTrans s input y (xs, len)
        case maybeNewState of
            Nothing -> return Nothing
            Just s'
              | len + n == pwLength -> return $ Just (s', x `cons` xs)
              | otherwise -> genPassword'
                $ if len + n > pwLength
                    then (s, xs, len)
                        -- Value was too large, discard it and get new one.
                    else (s', x `cons` xs, len + n)
