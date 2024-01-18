{-# Language OverloadedStrings #-}
module Main where

import Hakyll

import Hakyll.SkylightingCss (skylightingCssCompiler)

config :: Configuration
config = defaultConfiguration
    { destinationDirectory = "docs"
    , ignoreFile = \f -> case f of
                           ('.':x:_) -> x == '#'
                           _ -> ignoreFile defaultConfiguration f
    , providerDirectory = "content"
    }


staticFiles :: Rules ()
staticFiles = do
    -- just copy these over verbatim
    match common $ do
        route idRoute
        compile copyFileCompiler

    -- If a browser falls back to looking for favicon.ico, it always
    -- looks for it at the top level
    match "img/favicon.ico" $ do
        route $ constRoute "favicon.ico"
        compile copyFileCompiler
  where
    common = image .||. font .||. javascript
    font = "fonts/**" .&&. ("**.woff" .||. "**.woff2")
    javascript = "js/**" .&&. ("**.js" .||. "**.js.map")
    image = "img/**" .&&. ("**.gif" .||. "**.jpg" .||. "**.jpeg" .||. "**.png")


-- copy files required to be at the top-level as metadata for the
-- build/deploy/hosting systems
metaFiles :: Rules ()
metaFiles = match "*" $ do
    route idRoute
    compile copyFileCompiler


indexFile :: Rules ()
indexFile = match "posts/index.md" $ do
    route $ constRoute "index.html"
    compile $ do
        doc <- pandocCompiler
        loadAndApplyTemplate "templates/default.html" defaultContext doc


buildStylesheets :: Rules ()
buildStylesheets = do
    -- compress external stylesheets to css directory
    match "css/external/*.css" $ do
        route $ gsubRoute "/external/" (const "/")
        compile compressCssCompiler

    -- compress local static stylesheets
    match "css/*.css" $ do
        route idRoute
        compile compressCssCompiler

    -- generate CSS from the configured Skylighting theme
    create ["css/skylighting.css"] $ do
        route idRoute
        compile $ do
            style <- loadBody "config/skylighting-style"
            skylightingCssCompiler style

    -- combine all local CSS into one file
    create ["css/local.css"] $ do
        route idRoute
        compile $ do
            -- Prevent cyclic dependency. Doesn't bind external css
            -- because loading uses identifiers as matched, not as
            -- deployed with routes.
            let deps = "css/*.css" .&&. complement "css/local.css"
            cssContents <- map itemBody <$> loadAll deps
            makeItem (compressCss $ concat cssContents)


main :: IO ()
main = hakyllWith config $ do
    match "config/**" $ compile getResourceString
    match "templates/**" $ compile templateCompiler

    metaFiles
    staticFiles
    buildStylesheets
    indexFile
