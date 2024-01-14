{-# Language OverloadedStrings #-}
module Main where

import Hakyll

import Data.Char (toLower)
import Data.Traversable (for)

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
createMaterialSymbols = create ["css/material-symbols.css"] $ do
    route idRoute
    compile $ do
        tpl <- loadBody "templates/material-symbols.css"
        empty <- makeItem ()
        parts <- for ["Sharp", "Outlined", "Rounded"] $ \shape -> do
            let lshape = map toLower shape
                context = constField "shape" shape <>
                          constField "lshape" lshape
            applyTemplate tpl context empty
        let whole = concatMap itemBody parts
        makeItem $ compressCss whole


markdownToHTML :: Rules ()
markdownToHTML =
    match "*.md" $ do
        route $ setExtension ".html"
        compile $ do
            pandoc <- pandocCompiler
            loadAndApplyTemplate "templates/default.html" defaultContext pandoc


loadTemplates :: Rules ()
loadTemplates = match "templates/**" $ compile templateCompiler


main :: IO ()
main = hakyllWith config $ do
    loadTemplates
    createMaterialSymbols
    markdownToHTML
    staticFiles
