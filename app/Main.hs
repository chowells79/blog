{-# Language OverloadedStrings #-}
module Main where

import Data.Foldable (forM_)
import Data.List (nub)
import GHC.Stack (HasCallStack)

import Hakyll
import Hakyll.SkylightingCss (skylightingCssCompiler)

staticFiles :: HasCallStack => Rules ()
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


-- Generate files github treats specially at the top level
specialFiles :: HasCallStack => Rules ()
specialFiles = do
    match "CNAME" $ do
        route idRoute
        compile copyFileCompiler

    let tpl = "templates/default.html"
        applyDefault = loadAndApplyTemplate tpl defaultContext

    match ("index.md" .||. "404.md") $ do
        route $ setExtension "html"
        compile $ pandocCompiler >>= applyDefault


buildStylesheets :: HasCallStack => Rules ()
buildStylesheets = do
    -- compress external stylesheets to deploy directory
    match ("css/**" .&&. css) $ do
        route idRoute
        compile compressCssCompiler

    -- generate CSS from Skylighting files
    match ("css/**" .&&. sky) $ do
        route $ setExtension "css"
        compile $ do
            style <- itemBody <$> getResourceString
            skylightingCssCompiler style

    -- create concatenated CSS from subdirectory contents. (this is
    -- non-recursive due to not wanting to work really hard to bypass
    -- Hakyll limitations)
    let pat = "css/*/*" .&&. both
    dep <- makePatternDependency pat
    rulesExtraDependencies [dep] $ do
        idents <- getMatches pat
        let extractParent = maybe
                            (error "impossible stylesheet subdirectory mismatch")
                            (foldr const (error "impossible stylesheet subdirectory match"))
            dirs = nub . map (extractParent . capture "**/*") $ idents
        forM_ dirs $ \dirName -> do
            create [ fromFilePath $ dirName ++ ".css" ] $ do
                route idRoute
                compile $ do
                    let dirPattern = fromGlob (dirName ++ "/*") .&&. both
                    cssContents <- map itemBody <$> loadAll dirPattern
                    makeItem . compressCss . concat $ cssContents
  where
    css = "**.css"
    sky = "**.skylighting"
    both = css .||. sky

main :: HasCallStack => IO ()
main = do
    let config = defaultConfiguration { providerDirectory = "content"}
    hakyllWith config $ do
        match "templates/**" $ compile templateCompiler

        specialFiles
        staticFiles
        buildStylesheets
