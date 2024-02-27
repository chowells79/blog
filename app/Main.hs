module Main where

import Site (fullSite)
import Parser (BlogCommand(..), blogOptions, getOptions, hakyllConfig)

import Control.Monad (when)
import GHC.Stack (HasCallStack)

import Hakyll.Main (Options(..), hakyllWithArgs)


runCommand :: HasCallStack => Bool -> BlogCommand -> IO ()
runCommand verbose c = case c of
    Echo str -> do
        when verbose $ putStrLn "Doing an echo"
        putStrLn str
    Hakyll command ->
        hakyllWithArgs hakyllConfig (Options verbose command) fullSite


main :: HasCallStack => IO ()
main = do
    opts <- getOptions
    blogOptions runCommand opts
