{-# Language OverloadedStrings #-}
module Main where

import Hakyll

config :: Configuration
config = defaultConfiguration
    { destinationDirectory = "docs"
    , providerDirectory = "content"
    }

main :: IO ()
main = hakyllWith config $ do
    match "*.md" $ do
        route $ setExtension ".html"
        compile $ pandocCompiler >>=
            loadAndApplyTemplate "templates/default.html" defaultContext >>=
            relativizeUrls

    match "templates/*" $ compile templateCompiler

    match ("*.png" .||. "CNAME" .||. "favicon.ico" .||. "site.webmanifest")$ do
        route idRoute
        compile copyFileCompiler

    create [".nojekyll"] $ do
        route idRoute
        compile $ makeItem ("" :: String)
