-- |
-- Module:       Main
-- Description:  Command line tool that generates random passwords.
-- Copyright:    (c) 2013 Peter Trsko
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  portable
--
-- Command line tool that generates random passwords.
module Main (main)
    where

import Control.Applicative ((<$>))
import Control.Monad (replicateM)
import Data.Char (isDigit)
import Data.Maybe (fromMaybe)
import Data.Word (Word32)
import System.Environment (getArgs)

import Control.Monad.Random (evalRandIO, getRandomR)
import System.Console.Terminal.Size as Terminal (Window(..), size)

import Text.Pwgen
    ( GenPasswordConfig
    , alternatingConfigThreeState
    , genPassword
    , lowerConsonantsY
    , lowers
    , lowerVowelsY
    , numbers
    , simpleConfig
    , symbols
    , upperConsonantsY
    , uppers
    , upperVowelsY
    )

{-
config :: GenPasswordConfig () String String Char
config = simpleConfig (inputAlphabet, fromIntegral $ length inputAlphabet)
  where
    inputAlphabet :: [Char]
    inputAlphabet = numbers (:) . lowers (:) . uppers (:) . uppers (:)
        . lowers (:) $ symbols (:) []
-}

vowels :: [String]
vowels = lowerVowelsY (:) $ upperVowelsY (:)
    [ "ae", "ai", "ao", "au", "ea", "ee", "ei", "eo", "eu", "ia", "ie", "io"
    , "iu", "oa", "oi", "oo", "ou", "ua", "ui", "uo"

    , "Ae", "Ai", "Ao", "Au", "Ea", "Ee", "Ei", "Eo", "Eu", "Ia", "Ie", "Io"
    , "Iu", "Oa", "Oi", "Oo", "Ou", "Ua", "Ui", "Uo"
    ]

consonants :: [String]
consonants = lowerConsonantsY (:) $ upperConsonantsY (:)
    [ "bl", "ch", "dr", "ph", "sh", "th"
    , "Bl", "Ch", "Dr", "Ph", "Sh", "Th"

    , "chr", "thr", "sch", "scl", "scr", "shr", "spl", "spr", "str", "ght"
    , "cht"

    , "Chr", "Thr", "Sch", "Scl", "Scr", "Shr", "Spl", "Spr", "Str", "Ght"
    , "Cht"
    ]

configPronounceable
    :: GenPasswordConfig (Maybe Bool) ([String], [String]) String String
configPronounceable = alternatingConfigThreeState
    (vowels, length' vowels)
    (consonants, length' consonants)
    (++) elem (!) [] (++)
  where
    length' = fromIntegral . length
    xs ! n = let x = xs !! fromIntegral n in (x, length' x)

-- | Default length of password.
defaultPwlen :: Word32
defaultPwlen = 8

-- | Default number of lines.
defaultNumberOfLines :: Int
defaultNumberOfLines = 20

-- | Print passwords in columns.
printPasswords
    :: Int
    -- ^ Number of columns to print passwords in.
    -> [String]
    -- ^ List of generated passwords.
    -> IO ()
printPasswords _ [] = return ()
printPasswords n pws = do
    putStrLn $ unwords x
    printPasswords n xs
  where
    (x, xs) = splitAt n pws

-- | Calculate number of columns to print passwords in.
--
-- > n * pwlen + n - 1   <= terminalWidth
-- > n * (pwlen + 1) - 1 <= terminalWidth
-- >
-- >       terminalWidth + 1
-- > n <= -------------------
-- >           pwlen + 1
numberOfColumns
    :: Int
    -- ^ Password length.
    -> Maybe (Window Int)
    -- ^ Terminal size or 'Nothing' if not a terminal.
    -> Maybe Int
    -- ^ Number of columns to print passwords in or 'Nothing' if not a
    -- terminal.
numberOfColumns pwlen s = case s of
    Nothing -> Nothing
    Just Window{width = n}
      | n <= pwlen    -> Just 1
      | otherwise     -> Just $ case (n + 1) `div` (pwlen + 1) of
        d | d <= 1    -> 1
          | otherwise -> d

-- | Calculate number of passwords that should be generated.
numberOfPasswords
    :: Maybe Int
    -- ^ Number of columns to print passwords in or 'Nothing' if not a
    -- terminal.
    -> Maybe Int
    -- ^ Number of passwords to print as specified on command line or 'Nothing'
    -- if not specified.
    -> Int
numberOfPasswords Nothing  Nothing  = 1
numberOfPasswords _        (Just x) = x
numberOfPasswords (Just x) Nothing  = x * defaultNumberOfLines

main :: IO ()
main = do
    (pwlen, numOfPws) <- processArgs <$> getArgs
    cols <- numberOfColumns (fromIntegral pwlen) <$> Terminal.size
    -- let std = replicateM numOfPws
    --      $ genPassword config (getRandomR . (,) 0) pwlen
    let pron = replicateM (numberOfPasswords cols numOfPws)
            $ genPassword configPronounceable (getRandomR . (,) 0) pwlen
    evalRandIO pron >>= printPasswords (fromMaybe 1 cols)
  where
    processArgs :: [String] -> (Word32, Maybe Int)
    processArgs []        = (defaultPwlen, Nothing)
    processArgs [x]       = (readNum x, Nothing)
    processArgs (x1:x2:_) = (readNum x1, Just $ readNum x2)

    readNum :: Read a => String -> a
    readNum x
      | all isDigit x = read x
      | otherwise     = error "Expecting positive integer."
