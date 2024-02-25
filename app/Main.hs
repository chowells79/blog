module Main where

import Site (fullSite)

import GHC.Stack (HasCallStack)

import Hakyll.Main (Options, hakyllWithArgs, optionParser)
import Hakyll.Core.Configuration (defaultConfiguration, providerDirectory)

import Options.Applicative

data MyOpts = Echo String | Hakyll Options deriving (Show)

myOptsParser :: Parser Options -> Parser MyOpts
myOptsParser opt = Echo <$> e <|> Hakyll <$> opt
  where
    e = strOption (long "echo" <> help "Provide a value to echo")

main :: HasCallStack => IO ()
main = do
    let config = defaultConfiguration { providerDirectory = "content"}
        parser = helper <*> myOptsParser (optionParser config)
        parserInfo = info parser (fullDesc <> progDesc desc)
        desc = "blog - A static site compiler"
    opts <- execParser parserInfo
    case opts of
        Echo e -> putStrLn e
        Hakyll o -> hakyllWithArgs config o fullSite
