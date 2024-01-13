{-# Language OverloadedStrings #-}
module Main where

import Hakyll

config :: Configuration
config = defaultConfiguration
    { destinationDirectory = "docs"
    , ignoreFile = const False
    , providerDirectory = "content"
    }

staticFiles :: Pattern
staticFiles = foldr1 (.||.) [image, font, stylesheet, javascript, github]
  where
    font = "fonts/**" .&&. ("**.ttf" .||. "**.otf" .||. "**.woff" .||. "**.woff2")
    stylesheet = "css/**" .&&. "**.css"
    javascript = "js/**" .&&. "**.js"
    image = "**.gif" .||. "**.jpg" .||. "**.jpeg" .||. "**.png" .||. "favicon.ico"
    github = ".nojekyll" .||. "CNAME"


main :: IO ()
main = hakyllWith config $ do
    match "*.md" $ do
        route $ setExtension ".html"
        compile $ do
            c <- pandocCompiler
            fullPage <- loadAndApplyTemplate "templates/default.html" defaultContext c
            relativizeUrls fullPage

    match "templates/*" $ compile templateCompiler

    match staticFiles $ do
        route idRoute
        compile copyFileCompiler
