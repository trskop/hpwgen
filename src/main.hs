-- |
-- Module:       Main
-- Copyright:    (c) 2013 Peter Trsko
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
module Main (main)
    where

import Control.Monad (liftM2)
import System.Environment (getArgs, getProgName)

import Main.ApplicationMode (runApplication)
import Main.Application (runApp, processOptions)


main :: IO ()
main = do
    endos <- liftM2 processOptions getProgName getArgs
    runApplication endos (\ _ _ -> return Nothing) runApp
