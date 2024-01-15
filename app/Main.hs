{-# Language OverloadedStrings #-}
module Main where

import Hakyll

import SkylightingCss (skylightingCss)

config :: Configuration
config = defaultConfiguration
    { destinationDirectory = "docs"
    , ignoreFile = \f -> case f of
                           ('.':_) -> False
                           _ -> ignoreFile defaultConfiguration f
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
        route $ constRoute "favicon.ico"
        compile copyFileCompiler
  where
    common = image .||. font .||. javascript
    font = "fonts/**" .&&. ("**.woff" .||. "**.woff2")
    stylesheet = "css/**" .&&. "**.css"
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
        pandoc <- pandocCompiler
        loadAndApplyTemplate "templates/default.html" defaultContext pandoc


loadTemplates :: Rules ()
loadTemplates = match "templates/**" $ compile templateCompiler


loadConfigs :: Rules ()
loadConfigs = match "config/**" $ compile getResourceString


-- currently only magically regenerates the syntax highlighting style,
-- but there will probably be more
skylighting :: Rules ()
skylighting = create ["css/skylighting.css"] $ do
    route $ constRoute "css/skylighting.css"
    compile $ do
        style <- loadBody "config/skylighting-style"
        case skylightingCss style of
            Just css -> makeItem $ compressCss css
            Nothing -> noResult $
                "unable to generate syntax highlighting " ++
                "for the style named \"" ++ style ++ "\""


main :: IO ()
main = hakyllWith config $ do
    loadConfigs
    loadTemplates
    metaFiles
    staticFiles
    skylighting
    indexFile
