module Main where

import Site (fullSite)
import Parser (BlogOptions(..), BlogCommand(..), getOptions)

import Control.Monad (when)

import GHC.Stack (HasCallStack)

import Hakyll.Main (Options(..), hakyllWithArgs)
import Hakyll.Core.Configuration (defaultConfiguration, providerDirectory)



main :: HasCallStack => IO ()
main = do
    opts <- getOptions
    case opts of
        BlogOptions verbose c -> case c of
            Echo str -> do
                when verbose $ putStrLn "Doing an echo"
                putStrLn str
            Hakyll command -> do
                let config = defaultConfiguration
                             { providerDirectory = "content"}
                    hOpts = Options verbose command
                hakyllWithArgs config hOpts fullSite
