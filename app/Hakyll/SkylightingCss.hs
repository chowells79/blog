module Hakyll.SkylightingCss
  ( skylightingCssCompiler
  ) where

import Hakyll

import Skylighting.Format.HTML
import Skylighting.Styles


skylightingCss :: String -> Maybe String
skylightingCss name = styleToCss <$> style
  where
    style = case name of
              "kate"       -> Just kate
              "breezeDark" -> Just breezeDark
              "pygments"   -> Just pygments
              "espresso"   -> Just espresso
              "tango"      -> Just tango
              "haddock"    -> Just haddock
              "monochrome" -> Just monochrome
              "zenburn"    -> Just zenburn
              _ -> Nothing


skylightingCssCompiler :: String -> Compiler (Item String)
skylightingCssCompiler name =
    case skylightingCss shortName of
        Just css -> makeItem $ compressCss css
        Nothing -> noResult $ "unable to generate syntax highlighting " ++
                              "for the style named \"" ++ shortName ++ "\""
  where
    shortName = foldr const "<blank>" $ words name
