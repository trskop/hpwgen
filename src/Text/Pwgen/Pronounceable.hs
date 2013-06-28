{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module:       $HEADER$
-- Description:  Pronounceable passwords similar to what pwgen does.
-- Copyright:    (c) 2013 Peter Trsko
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  non-portable (OverloadedStrings)
--
-- Configuration for generating pronounceable passwords similar to what pwgen
-- <http://pwgen.sourceforge.net/> does.
module Text.Pwgen.Pronounceable
    (
    -- * GenPasswordConfig
      genPwConfig
    , genPwConfigBS
    )
    where

import Control.Applicative ((<$>))
import Data.Char (toLower, isUpper)
import Data.Maybe (fromMaybe)
import Data.Monoid (Monoid(mempty, mappend))
import Data.String (IsString)
import Data.Word (Word32)

import Control.Applicative.Predicate ((<||>), (>.), is)
import Data.Array (Array)
import qualified Data.Array.IArray as Array ((!), listArray)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS (any, head, length)
import Data.Default.Class (Default(def))

import Text.Pwgen (FromAscii, GenPasswordConfig(..))

import qualified Text.Pwgen as Pwgen
    ( isVowel
    , lowerConsonantsY
    , lowerVowels
    , numbers
    , symbols
    , upperConsonantsY
    , upperVowels
    )


-- | Character category. It's used to determine what kind of character we
-- currently want to add to password and what character should be added next.
data Category = Vowel | Consonant | Number | Symbol
    deriving (Eq)

data State = State
    { previousWas :: Maybe (Category, Bool)
    -- ^ What symbol was generated previously. 'Nothing' is used only as
    -- initial state.
    , nextShouldBe :: Maybe Category
    -- ^ What symbol should be added next. If 'Nothing', then it randomly
    -- chooses from vowels and consonants.
    , hasSymbol :: Bool
    -- ^ Password already has symbol in it, don't check again.
    , hasUpper :: Bool
    -- ^ Password already has upper character in it, don't check again.
    , hasNumber :: Bool
    -- ^ Password already has number in it, don't check again.
    }

instance Default State where
    def = State Nothing Nothing False False False

data Input t = Input
    { inVowels :: t
    , inUpperBoundOfVowels :: Word32
    , inConsonants :: t
    , inUpperBoundOfConsonants :: Word32
    , inNumbers :: t
    -- ^ If zero, then no numbers will be present in a generated password.
    , inUpperBoundOfNumbers :: Word32
    , inSymbols :: t
    , inUpperBoundOfSymbols :: Word32
    -- ^ If zero, then no symbols will be present in a generated password.
    , inCheckForUpperCharacters :: Bool
    -- ^ If 'False', then password won't be rejected if it doesn't contain
    -- upper character.
    }

-- | Function that simplifies creation of 'Input' values, while keeping a lot
-- of genericness.
mkInput
    :: (FromAscii u, IsString u)
    => (u -> Word32)
    -- ^ Get length of individual elements. It's then stored as the second item
    -- in a pair.
    -> (Word32 -> [(u, Word32)] -> t (u, Word32))
    -- ^ Construct e.g. array of @(u, Word32)@ pairs. First argument is the
    -- upper bound, counting from 0. Value of upper bound is also valid index.
    -> Bool
    -- ^ Include upper characters.
    -> Bool
    -- ^ Include numbers.
    -> Bool
    -- ^ Include symbols.
    -> Input (t (u, Word32))
mkInput uLength mkContainer withUppers withNumbers withSymbols = Input
    { inVowels = mkContainer vowelsUpperBound vowels
    , inUpperBoundOfVowels = vowelsUpperBound

    , inConsonants = mkContainer consonantsUpperBound consonants
    , inUpperBoundOfConsonants = consonantsUpperBound

    , inNumbers = mkContainer numbersUpperBound numbers'
    , inUpperBoundOfNumbers = if withNumbers then numbersUpperBound else 0

    , inSymbols = mkContainer symbolsUpperBound symbols'
    , inUpperBoundOfSymbols = if withSymbols then symbolsUpperBound else 0

    , inCheckForUpperCharacters = not withUppers
    }
  where
    length' = fromIntegral . length

    vowels = map (\ x -> (x, uLength x)) . lowerVowels . lowerVowels
        . lowerVowels . (if withUppers then upperVowels else id) $ []
    consonants = map (\ x -> (x, uLength x)) . lowerConsonants
        . lowerConsonants . lowerConsonants
        . (if withUppers then upperConsonants else id) $ []
    numbers' = map (\ x -> (x, 1)) $ Pwgen.numbers (:) []
    symbols' = map (\ x -> (x, 1)) $ Pwgen.symbols (:) []

    vowelsUpperBound = length' vowels - 1
    consonantsUpperBound = length' consonants - 1
    numbersUpperBound = length' numbers' - 1
    symbolsUpperBound = length' symbols' - 1

    append :: [a] -> [a] -> [a]
    append = foldl (\ f -> (f .) . (:)) id

    lowerVowels :: (FromAscii a, IsString a) => [a] -> [a]
    lowerVowels = Pwgen.lowerVowels (:) . Pwgen.lowerVowels (:)
        . append ["ae", "ah", "ai", "ee", "ei", "ie", "oh", "oo"]

    lowerConsonants :: (FromAscii a, IsString a) => [a] -> [a]
    lowerConsonants = Pwgen.lowerConsonantsY (:) . Pwgen.lowerConsonantsY (:)
        . append ["ch", "gh", "ng", "ph", "qu", "sh", "th"]

    upperVowels :: (FromAscii a, IsString a) => [a] -> [a]
    upperVowels = Pwgen.upperVowels (:) . append
        [ "Ae", "aE", "Ah", "aH", "Ai", "aI", "Ee", "eE"
        , "Ei", "eI", "Ie", "iE", "Oh", "oH", "Oo", "oO"
        ]

    upperConsonants :: (FromAscii a, IsString a) => [a] -> [a]
    upperConsonants = Pwgen.upperConsonantsY (:) . append
        [ "Ch", "cH", "Gh", "gH", "Ng", "nG", "Ph"
        , "pH", "Qu", "qU", "Sh", "sH", "Th", "tH"
        ]

-- | Generic configuration that doesn't rely on any particular string
-- implementation.
genPwConfig'
    :: (Monoid u)
    => (u -> Char)
    -- ^ Head function for @u@.
    -> (u -> Bool)
    -- ^ Function that checks if any character in @u@ is upper case.
    -> (t (u, Word32) -> Word32 -> (u, Word32))
    -- ^ Index function for @t@ container with @(u, Word32)@ elements.
    -> Input (t (u, Word32))
    -- ^ Input symbol sets, see 'Input' for details.
    -> GenPasswordConfig State (Input (t (u, Word32))) u u
genPwConfig' head' isUpper' (!) input = GenPasswordConfig
    { genPwInIndex = index
    , genPwInMaxIndex = maxIndex
    , genPwStateTransformation = stateTransformation
    , genPwIn = input
    , genPwInitialState = initialState
    , genPwOutEmpty = mempty
    , genPwOutCons = flip mappend -- Concatenate in reverse order.
    , genPwOutCond = \ s _ -> (hasNumber <||> hasSymbol <||> hasUpper) s
    }
  where
    -- :: t -> (s, Word32) -> (a, Word32)
    index t (s, n) = case nextShouldBe s of
        -- In case of 'Nothing' we are randomly choosing from both, consonants
        -- and vowels.
        Nothing
          | n <= inUpperBoundOfVowels t -> inVowels t ! n
          | otherwise -> inConsonants t ! (n - inUpperBoundOfVowels t - 1)
        Just Vowel -> inVowels t ! n
        Just Consonant -> inConsonants t ! n
        Just Number -> inNumbers t ! n
        Just Symbol -> inSymbols t ! n

    -- :: s -> t -> Word32
    maxIndex s t = case nextShouldBe s of
        Nothing -> inUpperBoundOfVowels t + inUpperBoundOfConsonants t + 1
        Just Vowel -> inUpperBoundOfVowels t
        Just Consonant -> inUpperBoundOfConsonants t
        Just Number -> inUpperBoundOfNumbers t
        Just Symbol -> inUpperBoundOfSymbols t

    initialState = def
        { hasUpper = inCheckForUpperCharacters input
        , hasNumber = inUpperBoundOfNumbers input == 0
        , hasSymbol = inUpperBoundOfSymbols input == 0
        }

    -- :: (Applicative m, Functor m, Monad m)
    -- => (Word32 -> m Word32) -> s -> t -> (a, Word32) -> (u, Word32)
    -- -> m (Maybe s)
    stateTransformation genRand s t (xs, xlen) _ = do
        nextShouldBeNumber <- if inUpperBoundOfNumbers t == 0
            then return False   -- Numbers in password are disabled.
            else 3 >. genRand 10
        nextShouldBeSymbol <- if inUpperBoundOfSymbols t == 0
            then return False   -- Symbols in password are disabled.
            else 2 >. genRand 10
        let x = head' xs
            currentIs =
                ( fromMaybe (if Pwgen.isVowel x then Vowel else Consonant)
                    $ nextShouldBe s
                , xlen > 0
                )
        case currentIs of
            (Consonant, isDipthong)
              | isDipthong && (is 'g' <||> is 'n') (toLower x) -> return Nothing
                -- "ng" and "gh" aren't allowed to be first in the password.
              | nextShouldBeNumber -> updateState s xs currentIs (Just Number)
              | nextShouldBeSymbol -> updateState s xs currentIs (Just Symbol)
              | otherwise -> updateState s xs currentIs (Just Vowel)
            (Vowel, isDipthong)
              | isDipthong && (fst <$> previousWas s) == Just Vowel ->
                return Nothing
              | nextShouldBeNumber -> updateState s xs currentIs (Just Number)
              | nextShouldBeSymbol -> updateState s xs currentIs (Just Symbol)
              | (fst <$> previousWas s) == Just Vowel || isDipthong ->
                updateState s xs currentIs (Just Consonant)
              | otherwise -> do
                nextShouldBeConsonant <- 3 >. genRand 10
                updateState s xs currentIs . Just
                    $ if nextShouldBeConsonant then Consonant else Vowel
            (Number, _) -> updateState s xs currentIs Nothing
            (Symbol, _)
              | (fst <$> previousWas s) == Just Vowel ->
                updateState s xs currentIs (Just Vowel)
              | otherwise -> do
                nextShouldBeConsonant <- 3 >. genRand 10
                updateState s xs currentIs . Just
                    $ if nextShouldBeConsonant then Consonant else Vowel

    updateState s xs currentIs@(cat, _) next = return $ Just State
        { previousWas = Just currentIs
        , nextShouldBe = next
        , hasSymbol = hasSymbol s || cat == Symbol
        , hasUpper = hasUpper s || isUpper' xs
        , hasNumber = hasNumber s || cat == Number
        }

genPwConfigBS
    :: Bool
    -- ^ Include upper characters.
    -> Bool
    -- ^ Include numbers.
    -> Bool
    -- ^ Include symbols.
    -> GenPasswordConfig State (Input (Array Word32 (ByteString, Word32)))
        ByteString ByteString
genPwConfigBS = ((genPwConfig' BS.head (BS.any isUpper) (Array.!) .) .)
    . mkInput (fromIntegral . BS.length) (Array.listArray . (,) 0)

genPwConfig
    :: Bool
    -- ^ Include upper characters.
    -> Bool
    -- ^ Include numbers.
    -> Bool
    -- ^ Include symbols.
    -> GenPasswordConfig State (Input (Array Word32 (String, Word32)))
        String String
genPwConfig = ((genPwConfig' head (any isUpper) (Array.!) .) .)
    . mkInput (fromIntegral . length) (Array.listArray . (,) 0)
