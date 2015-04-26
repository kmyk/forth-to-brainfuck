module Main where

import System.Process
import System.Exit
import Paths_forth_to_brainfuck (getDataFileName)

execProcess a b c d e f g = runProcess a b c d e f g >>= waitForProcess >>= exitWith
main :: IO ()
main = do
    dir <- getDataFileName "test"
    sh  <- getDataFileName "test/run.sh"
    execProcess sh [] (Just dir) Nothing Nothing Nothing Nothing
