{-
 - Main.hs
 - By Steven Smith
 -}

module Main where

import System.Environment

import SpirV.Builder (buildModule)
import SpirV.PrettyPrint

import Program1
import Program2

main :: IO ()
main = do
    args <- getArgs
    let program = case args of
            ["1"] -> program1
            ["2"] -> program2
            _ -> program1
    printModule (buildModule program)
