module Parser
  ( BlogOptions(..)
  , BlogCommand(..)
  , getOptions
  ) where


import Data.List (intercalate)
import GHC.Stack (HasCallStack)

import Hakyll.Main (Command, defaultCommands)
import Hakyll.Core.Configuration (defaultConfiguration)

import Options.Applicative


data BlogOptions = BlogOptions Bool BlogCommand deriving Show
data BlogCommand = Echo String | Hakyll Command deriving Show


commandParser :: HasCallStack => Parser BlogCommand
commandParser = subparser $ foldMap toCommand commands
  where
    toCommand (a, b, c) = command a $ info (helper <*> b) c
    commands = blogCommands ++ hakyllCommands
    hakyllCommands =
        [ (a, Hakyll <$> b, c)
        | (a, b, c) <- defaultCommands defaultConfiguration
        ]
    blogCommands =
        [ ( "echo"
          , Echo . intercalate " " <$> many (argument str mempty)
          , fullDesc <> progDesc "Demo command"
          )
        ]


optionParser :: HasCallStack => Parser BlogOptions
optionParser = BlogOptions <$> verboseParser <*> commandParser
  where
    verboseParser = switch mods
    mods = (long "verbose" <> short 'v' <> help "Run in verbose mode")


getOptions :: HasCallStack => IO BlogOptions
getOptions = do
    let desc = "blog - A static site compiler"
        parserInfo = info (helper <*> optionParser) (fullDesc <> progDesc desc)
    execParser parserInfo

