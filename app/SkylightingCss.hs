module SkylightingCss
  ( skylightingCss
  ) where

import Data.Char (isSpace)
import Data.List (dropWhileEnd)

import Skylighting.Format.HTML
import Skylighting.Styles


skylightingCss :: String -> Maybe String
skylightingCss name = styleToCss <$> style
  where
    stripped = dropWhile isSpace . dropWhileEnd isSpace $ name
    style = case stripped of
              "kate"       -> Just kate
              "breezeDark" -> Just breezeDark
              "pygments"   -> Just pygments
              "espresso"   -> Just espresso
              "tango"      -> Just tango
              "haddock"    -> Just haddock
              "monochrome" -> Just monochrome
              "zenburn"    -> Just zenburn
              _ -> Nothing
