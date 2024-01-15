{-# Language OverloadedStrings #-}
module Main where

import Hakyll

config :: Configuration
config = defaultConfiguration
    { destinationDirectory = "docs"
    , ignoreFile = \f -> case f of ('.':_) -> False ; _ -> ignoreFile defaultConfiguration f
    , providerDirectory = "content"
    }


staticFiles :: Rules ()
staticFiles = do
    -- just copy these over verbatim
    match common $ do
        route idRoute
        compile copyFileCompiler

    -- compress stylesheets
    match stylesheet $ do
        route idRoute
        compile compressCssCompiler

    -- If a browser falls back to looking for favicon.ico, it always
    -- looks for it at the top level
    match "img/favicon.ico" $ do
        route $ gsubRoute "img/" (const "")
        compile copyFileCompiler
  where
    common = image .||. font .||. javascript
    font = "fonts/**" .&&. ("**.woff" .||. "**.woff2")
    stylesheet = "css/**" .&&. "**.css"
    javascript = "js/**" .&&. "**.js"
    image = "img/**" .&&. ("**.gif" .||. "**.jpg" .||. "**.jpeg" .||. "**.png")


-- create files required to be at the top-level as metadata for the
-- build/deploy/hosting systems
metaFiles :: Rules ()
metaFiles = match ("meta/*") $ do
    route $ gsubRoute "meta/" (const "")
    compile copyFileCompiler


indexFile :: Rules ()
indexFile = match "posts/index.md" $ do
    route $ constRoute "index.html"
    compile $ do
        pandoc <- pandocCompiler
        loadAndApplyTemplate "templates/default.html" defaultContext pandoc


loadTemplates :: Rules ()
loadTemplates = match "templates/**" $ compile templateCompiler


main :: IO ()
main = hakyllWith config $ do
    loadTemplates
    metaFiles
    indexFile
    staticFiles
