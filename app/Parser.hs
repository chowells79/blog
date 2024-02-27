module Parser
  ( BlogOptions(..)
  , BlogCommand(..)
  , blogOptions
  , getOptions
  , hakyllConfig
  ) where


import Data.List (intercalate)
import GHC.Stack (HasCallStack)

import Hakyll.Main (Command, defaultCommands)
import Hakyll.Core.Configuration (Configuration(..), defaultConfiguration)

import Control.Lens hiding (argument)

import Options.Applicative


data BlogOptions = BlogOptions Bool BlogCommand deriving Show
data BlogCommand = Echo String | Hakyll Command deriving Show


-- fold for BlogOptions
blogOptions :: (Bool -> BlogCommand -> r) -> BlogOptions -> r
blogOptions f (BlogOptions v c) = f v c


hakyllConfig :: Configuration
hakyllConfig = defaultConfiguration { providerDirectory = "content" }



---------------------------------------------------------------------
-- logic for remaining definitions adapted from Hakyll definitions --
---------------------------------------------------------------------


commandParser :: HasCallStack => Parser BlogCommand
commandParser = subparser $ foldMap toCommand commands
  where
    toCommand (a, b, c) = command a $ info (helper <*> b) c
    commands = blogCommands ++ hakyllCommands
    hakyllCommands = rawCommands & mapped . _2 . mapped %~ Hakyll
    rawCommands = defaultCommands hakyllConfig
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
    mods = long "verbose" <> short 'v' <> help "Run in verbose mode"


getOptions :: HasCallStack => IO BlogOptions
getOptions = do
    let desc = "blog - A static site compiler"
        parserInfo = info (helper <*> optionParser) (fullDesc <> progDesc desc)
    execParser parserInfo

