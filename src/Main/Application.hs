{-# LANGUAGE MultiParamTypeClasses #-}
-- |
-- Module:       $HEADER$
-- Description:  Command line tool that generates random passwords.
-- Copyright:    (c) 2013 Peter Trsko
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  non-portable (FlexibleContexts, depends on non-portable
--               module)
--
-- Command line tool that generates random passwords.
module Main.Application (runApp, processOptions)
    where

import Control.Applicative ((<$>))
import Control.Monad (replicateM)
import Data.Char (isDigit)
import Data.Maybe (fromMaybe)
import Data.Version (Version)
import Data.Word (Word32)
import System.Console.GetOpt
    ( OptDescr(Option)
    , ArgDescr(NoArg)
    , ArgOrder(Permute)
    , getOpt
    )
import System.Exit (exitFailure)
import System.IO (Handle, stderr, stdout)

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS (hPutStrLn, unwords)
import Data.Default.Class (Default(def))
import Data.Monoid.Endo hiding ((<>))
import Data.Semigroup (Semigroup((<>)))
import System.Console.Terminal.Size as Terminal (Window(..), size)
import System.Console.GetOpt.UsageInfo
    ( UsageInfoConfig(outputHandle)
    , renderUsageInfo
    )

import Main.ApplicationMode
    ( ApplicationMode(..)
    , SimpleMode(..)
    , changeAction
    , updateConfiguration
    )
import Main.ApplicationMode.SimpleAction (SimpleAction(..))
import qualified Main.ApplicationMode.SimpleAction as SimpleAction (optErrors)
import Main.Common (Parameters(..), printHelp, printVersion, printOptErrors)
import Main.MiniLens (E, L, get, mkL, set)
import Main.Random (withGenRand)
import qualified Text.Pwgen.Pronounceable as Pronounceable (genPwConfigBS)
import Text.Pwgen (genPassword)

import Paths_hpwgen (version)


-- | Default length of password.
defaultPwlen :: Word32
defaultPwlen = 8

-- | Default number of lines, this is used when printing passwords in columns,
-- but number of passwords wasn't specified.
defaultNumberOfLines :: Int
defaultNumberOfLines = 20

-- | Line length used when printing in columns, but to a handle that isn't a
-- terminal.
defaultLineLength :: Int
defaultLineLength = 80

type HpwgenMode = SimpleMode SimpleAction Config

instance ApplicationMode SimpleMode SimpleAction Config where
    optErrors msgs = case SimpleAction.optErrors msgs of
        Nothing -> mempty
        Just a -> changeAction a
            `mappend` updateConfiguration (set outHandleL stderr)

data Config = Config
    { cfgProgName :: String
    , cfgVersion :: Version
    , cfgPasswordLength :: Word32
    , cfgNumberOfPasswords :: Maybe Int
    , cfgPrintInColumns :: Maybe Bool
    -- ^ If 'Nothing' then it's determined depending on the fact if output is
    -- a terminal. When 'True' and output is not a terminal 80 character width
    -- is assumed. And when 'False' then only one password is printed per line.
    , cfgGeneratePronounceable :: Bool
    , cfgIncludeNumbers :: Bool
    , cfgIncludeSymbols :: Bool
    , cfgIncludeUppers :: Bool
    , cfgOutHandle :: Handle
    }

instance Default Config where
    def = Config
        { cfgProgName = ""
        , cfgVersion = version
        , cfgPasswordLength = defaultPwlen
        , cfgNumberOfPasswords = Nothing
        , cfgPrintInColumns = Nothing
        , cfgGeneratePronounceable = True
        , cfgIncludeNumbers = True
        , cfgIncludeSymbols = True
        , cfgIncludeUppers = True
        , cfgOutHandle = stdout
        }

outHandleL :: L Config Handle
outHandleL = mkL cfgOutHandle $ \ h c -> c{cfgOutHandle = h}

progNameL :: L Config String
progNameL = mkL cfgProgName $ \ pn c -> c{cfgProgName = pn}

generatePronounceableL :: L Config Bool
generatePronounceableL =
    mkL cfgGeneratePronounceable $ \ b c -> c{cfgGeneratePronounceable = b}

passwordLengthL :: L Config Word32
passwordLengthL = mkL cfgPasswordLength $ \ n c -> c{cfgPasswordLength = n}

numberOfPasswordsL :: L Config (Maybe Int)
numberOfPasswordsL =
    mkL cfgNumberOfPasswords $ \ n c -> c{cfgNumberOfPasswords = n}

includeNumbersL :: L Config Bool
includeNumbersL = mkL cfgIncludeNumbers $ \ b c -> c{cfgIncludeNumbers = b}

includeSymbolsL :: L Config Bool
includeSymbolsL = mkL cfgIncludeSymbols $ \ b c -> c{cfgIncludeSymbols = b}

includeUppersL :: L Config Bool
includeUppersL = mkL cfgIncludeUppers $ \ b c -> c{cfgIncludeUppers = b}

printInColumnsL :: L Config (Maybe Bool)
printInColumnsL = mkL cfgPrintInColumns $ \ b c -> c{cfgPrintInColumns = b}

params :: Parameters Config
params = def
    { paramOutputHandle = get outHandleL
    , paramProgName = get progNameL
    , paramCommand = get progNameL
    , paramVersion = cfgVersion
    , paramUsage = const
        [ "[OPTIONS] [PASSWORD_LENGTH [NUMBER_OF_PASSWORDS]]"
        , "{-h|--help|-V|--version|--numeric-version}"
        ]
    }

options :: [OptDescr (Endo HpwgenMode)]
options =
{- TODO
    [ Option "s" ["secure"]
        (NoArg . updateConfiguration $ set generatePronounceableL False)
        "Generate completely random passwords."
-}
    [ Option "cu" ["capitalize", "upper"]
        (NoArg . updateConfiguration $ set includeUppersL True)
        $ defaultMark (get includeUppersL)
            "Upper case letters will be included in passwords."
    , Option "CU" ["no-capitalize", "no-upper"]
        (NoArg . updateConfiguration $ set includeUppersL False)
        $ defaultMark (not . get includeUppersL)
            "Upper case letters won't be included in passwords."
    , Option "n"  ["numerals", "numbers"]
        (NoArg . updateConfiguration $ set includeNumbersL True)
        $ defaultMark (get includeNumbersL)
            "Numbers will be included in passwords."
    , Option "N0" [ "no-numerals", "no-numbers"]
        (NoArg . updateConfiguration $ set includeNumbersL False)
        $ defaultMark (not . get includeNumbersL)
            "Numbers won't be included in passwords."
    , Option "y"  [ "symbols"]
        (NoArg . updateConfiguration $ set includeSymbolsL True)
        $ defaultMark (get includeSymbolsL)
            "Special symbols will be included in passwords."
    , Option "Y"  [ "no-symbols"]
        (NoArg . updateConfiguration $ set includeSymbolsL False)
        $ defaultMark (not . get includeSymbolsL)
            "Special symbols won't be included in passwords."
    , Option "1"  ["one-column"]
        (NoArg . updateConfiguration . set printInColumnsL $ Just False)
        "Passwords will be printed in only one column. If number of passwords\
        \ is not specified, then only one password will be printed."
    , Option "h"  ["help"]
        (NoArg $ changeAction PrintHelp)
        "Print this help and exit."
    , Option "V"  ["version"]
        (NoArg . changeAction $ PrintVersion False)
        "Print version number and exit."
    , Option ""   ["numeric-version"]
        (NoArg . changeAction $ PrintVersion True)
        "Print version number (numeric form only) and exit. Useful for batch\
        \ processing."
    ]
  where
    defaultMark :: (Config -> Bool) -> String -> String
    defaultMark p
      | p def     = (++ " (default)")
      | otherwise = id

processOptions :: String -> [String] -> Endo HpwgenMode
processOptions progName = mconcat . processOptions' . getOpt Permute options
  where
    processOptions' (endos, nonOpts, errs) =
        optErrors errs
        : processNonOpts nonOpts
        : updateConfiguration (set progNameL progName)
        : reverse endos

    setNum :: Read a => (a -> E Config) -> String -> String -> Endo HpwgenMode
    setNum setter what s
      | all isDigit s = updateConfiguration . setter $ read s
      | otherwise     = optError
        $ "Incorrect " ++ what ++ ": Expecting number, but got: " ++ show s

    setPwNum, setPwLen :: String -> Endo HpwgenMode
    setPwNum = setNum (set numberOfPasswordsL . Just) "number of passwords"
    setPwLen = setNum (set passwordLengthL) "password length"

    processNonOpts opts = case opts of
        [] -> mempty
        [n] -> setPwLen n
        [n, m] -> setPwLen n <> setPwNum m
        _ -> optError . ("Too many options: " ++) . unwords $ drop 2 opts

-- | Print passwords in columns.
printPasswords
    :: Handle
    -> Int
    -- ^ Number of columns to print passwords in.
    -> [ByteString]
    -- ^ List of generated passwords.
    -> IO ()
printPasswords _ _ [] = return ()
printPasswords h n pws = do
    BS.hPutStrLn h $ BS.unwords x
    printPasswords h n xs
  where
    (x, xs) = splitAt n pws

-- | Calculate number of columns to print passwords in and number of passwords
-- that should be generated.
numberOfColumnsAndPasswords
    :: Config
    -> Maybe Int
    -- ^ Terminal width or 'Nothing' if not a terminal.
    -> (Int, Int)
    -- ^ Number of columns to print passwords in and number of passowrds that
    -- will be generated.
numberOfColumnsAndPasswords cfg s = case (get printInColumnsL cfg, s) of
    (Nothing, Nothing) -> (1, fromMaybe 1 pwNum)
        -- In auto mode and output is not a terminal.
    (Just False, _) -> (1, fromMaybe 1 pwNum)
        -- Forcing one column mode, then by default just one password shouls be
        -- printed.
    (Just True, Nothing) ->  let cols = numberOfColumns defaultLineLength
        in (cols, fromMaybe (cols * defaultNumberOfLines) pwNum)
        -- Forced to print in columns, but output is not a terminal, assuming
        -- defaultLineLength character width.
    (_, Just n) -> let cols = numberOfColumns n
        in (cols, fromMaybe (cols * defaultNumberOfLines) pwNum)
        -- Either in auto mode or forced to print in columns, output is a
        -- terminal.
  where
    pwNum = fromIntegral <$> get numberOfPasswordsL cfg

    -- n * pwlen + n - 1   <= terminalWidth
    -- n * (pwlen + 1) - 1 <= terminalWidth
    --
    --       terminalWidth + 1
    -- n <= -------------------
    --           pwlen + 1
    numberOfColumns n
      | n <= pwlen    = 1
      | otherwise     = case (n + 1) `div` (pwlen + 1) of
        d | d <= 1    -> 1
          | otherwise -> d
      where
        pwlen = fromIntegral $ get passwordLengthL cfg

runApp :: SimpleAction -> Config -> IO ()
runApp a cfg = case a of
    PrintVersion numericOnly -> printVersion' numericOnly
    PrintHelp -> printHelp'
    OptErrors errs -> printOptErrors' errs >> printHelp' >> exitFailure
    Action -> generatePasswords cfg
  where
    withParams f = f params cfg
    printVersion' = withParams printVersion
    printOptErrors' = withParams printOptErrors
    printHelp' = do
        str <- renderUsageInfo usageInfoCfg "" options
        withParams printHelp (\ _ _ -> unlines [str])
      where usageInfoCfg = def{outputHandle = get outHandleL cfg}

generatePasswords :: Config -> IO ()
generatePasswords cfg = do
    (cols, pwNum) <- numberOfColumnsAndPasswords cfg . fmap width
        <$> Terminal.size
    printPasswords handle cols =<< withGenRand (\ genRandom ->
        replicateM pwNum $ genPassword genPwCfg genRandom pwLen)
  where
    pwLen = get passwordLengthL cfg
    handle = get outHandleL cfg

    genPwCfg = (if get generatePronounceableL cfg
        then Pronounceable.genPwConfigBS
        else Pronounceable.genPwConfigBS)
            (get includeUppersL cfg)
            (get includeNumbersL cfg)
            (get includeSymbolsL cfg)
    -- secureCfg cfg =
