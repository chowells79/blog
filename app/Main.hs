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
staticFiles = do
    match common $ do
        route idRoute
        compile copyFileCompiler

    -- If a browser falls back to looking for favicon.ico, it always
    -- looks for it at the top level
    match "img/favicon.ico" $ do
        route $ gsubRoute "img/" (const "")
        compile copyFileCompiler
  where
    common = foldr1 (.||.) [image, font, stylesheet, javascript]
    font = "fonts/**" .&&. ("**.ttf" .||. "**.otf" .||. "**.woff" .||. "**.woff2")
    stylesheet = "css/**" .&&. "**.css"
    javascript = "js/**" .&&. "**.js"
    image = "img/**" .&&. ("**.gif" .||. "**.jpg" .||. "**.jpeg" .||. "**.png")


-- create files required to be at the top-level as metadata for the
-- build/deploy/hosting systems
metaFiles :: Rules ()
metaFiles = match ("meta/**") $ do
    route $ gsubRoute "meta/" (const "")
    compile copyFileCompiler


indexFile :: Rules ()
indexFile = match "posts/index.md" $ do
    route $ constRoute "index.html"
    compile $ do
        pandoc <- pandocCompiler
        loadAndApplyTemplate "templates/default.html" defaultContext pandoc


-- creates CSS for loading all three variants of the symbol set
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


loadTemplates :: Rules ()
loadTemplates = match "templates/**" $ compile templateCompiler


main :: IO ()
main = hakyllWith config $ do
    loadTemplates
    metaFiles
    indexFile
    staticFiles
    createMaterialSymbols
