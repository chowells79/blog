{-# Language OverloadedStrings #-}
module Main where

import Hakyll

import Hakyll.SkylightingCss (skylightingCssCompiler)

config :: Configuration
config = defaultConfiguration
    { ignoreFile = \f -> case f of
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
    common = images .||. fonts .||. scripts
    fonts = "fonts/**" .&&. ("**.woff" .||. "**.woff2")
    scripts = "js/**" .&&. ("**.js" .||. "**.js.map")
    images = "img/**" .&&. ("**.gif" .||. "**.jpg" .||. "**.png")


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
    -- compress external stylesheets to deploy directory
    match "css/*.css" $ do
        route idRoute
        compile compressCssCompiler

    -- generate CSS from the configured Skylighting theme
    create ["css/local/skylighting.css"] $ do
        route $ constRoute "css/skylighting.css"
        compile $ do
            style <- loadBody "config/skylighting-style"
            skylightingCssCompiler style

    -- compress local static stylesheets
    match "css/local/*.css" $ do
        route $ gsubRoute "local/" (const "")
        compile compressCssCompiler

    -- combine all local CSS into one file
    create ["css/local.css"] $ do
        route idRoute
        compile $ do
            cssContents <- map itemBody <$> loadAll "css/local/*.css"
            makeItem (compressCss $ concat cssContents)


main :: IO ()
main = hakyllWith config $ do
    match "config/**" $ compile getResourceString
    match "templates/**" $ compile templateCompiler

    metaFiles
    staticFiles
    buildStylesheets
    indexFile
