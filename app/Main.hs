{-# Language OverloadedStrings #-}
module Main where

import Hakyll

import Data.Foldable (for_)
import Data.Char (toLower)
import Data.String (IsString(..))

config :: Configuration
config = defaultConfiguration
    { destinationDirectory = "docs"
    , ignoreFile = \f -> case f of ('.':_) -> False ; _ -> ignoreFile defaultConfiguration f
    , providerDirectory = "content"
    }


staticFiles :: Rules ()
staticFiles =  match pat $ do
    route idRoute
    compile copyFileCompiler
  where
    pat = foldr1 (.||.) [image, font, stylesheet, javascript, github]
    font = "fonts/**" .&&. ("**.ttf" .||. "**.otf" .||. "**.woff" .||. "**.woff2")
    stylesheet = "css/**" .&&. "**.css"
    javascript = "js/**" .&&. "**.js"
    image = "**.gif" .||. "**.jpg" .||. "**.jpeg" .||. "**.png" .||. "favicon.ico"
    github = ".nojekyll" .||. "CNAME"


createMaterialSymbols :: Rules ()
createMaterialSymbols = for_ ["Sharp", "Outlined", "Rounded"] $ \shape -> do
    let name = "css/material-symbols-" ++ map toLower shape ++ ".css"
    create [fromString name] $ do
        route idRoute
        compile $ do
            let context = constField "shape" shape
            empty <- makeItem ("" :: String)
            loadAndApplyTemplate "templates/material-symbols.css" context empty


markdownToHTML :: Rules ()
markdownToHTML =
    match "*.md" $ do
        route $ setExtension ".html"
        compile $ do
            pandoc <- pandocCompiler
            fullPage <- loadAndApplyTemplate "templates/default.html" defaultContext pandoc
            relativizeUrls fullPage


loadTemplates :: Rules ()
loadTemplates = match "templates/**" $ compile templateCompiler


main :: IO ()
main = hakyllWith config $ do
    loadTemplates
    createMaterialSymbols
    markdownToHTML
    staticFiles
