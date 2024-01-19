{-# Language OverloadedStrings #-}
module Main where

import Hakyll

import Hakyll.SkylightingCss (skylightingCssCompiler)

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
metaFiles = do
    match "*" $ do
        route idRoute
        compile copyFileCompiler


-- build HTML files that github pages treat specially
specialHTML :: Rules ()
specialHTML = do
    let (&.) = composeRoutes
        tpl = "templates/default.html"
        applyDefault = loadAndApplyTemplate tpl defaultContext
    match ("posts/index.md" .||. "posts/404.md") $ do
        route $ gsubRoute "posts/" (const "") &. setExtension "html"
        compile $ pandocCompiler >>= applyDefault


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
main = do
    let config = defaultConfiguration { providerDirectory = "content"}
    hakyllWith config $ do
        match "config/**" $ compile getResourceString
        match "templates/**" $ compile templateCompiler

        metaFiles
        staticFiles
        buildStylesheets
        specialHTML
