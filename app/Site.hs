{-# Language OverloadedStrings #-}
module Site
  ( fullSite
  ) where

import Data.Foldable (forM_)
import Data.List (inits, intercalate, nub)
import Data.List.Split (splitOn)
import GHC.Stack (HasCallStack)

import SiteVersion

import Hakyll
import Hakyll.SkylightingCss (skylightingCssCompiler)


copiedFiles :: HasCallStack => Rules ()
copiedFiles = do
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


-- Generate various one-off files.
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

    create ["site_version.json"] $ do
        route idRoute
        compile $ makeItem =<< recompilingUnsafeCompiler lookupSiteVersion


buildStylesheets :: HasCallStack => Rules ()
buildStylesheets = version "css" $ do
    -- compress static stylesheets to deploy directory
    let cssFiles = "css/**.css"
    match cssFiles $ do
        route idRoute
        compile compressCssCompiler

    -- generate CSS from Skylighting files
    let skyFiles = "css/**.skylighting"
    match skyFiles $ do
        route $ setExtension "css"
        compile $ do
            style <- itemBody <$> getResourceString
            skylightingCssCompiler style

    ------------------------------------------------------
    -- create concatenated CSS from subdirectory contents.

    -- match all style *files* in subdirectories
    let pat = cssFiles .||. skyFiles
    dep <- makePatternDependency pat
    rulesExtraDependencies [dep] $ do
        idents <- getMatches pat
        let nothingErr = error "impossible stylesheet subdirectory mismatch"
            emptyErr = error "impossible stylesheet subdirectory match"
            extractParents = maybe nothingErr $ foldr const emptyErr
            -- generate all subdirectories as individual entries in
            -- case there are some that contain no files
            multiply = map (intercalate "/") . drop 2 . inits . splitOn "/"
            dirs = nub . concatMap (multiply . extractParents . capture "**/*") $ idents
        forM_ dirs $ \dirName -> do
            create [ fromFilePath $ dirName ++ ".css" ] $ do
                route idRoute
                compile $ do
                    -- loads *compiled* css resources in the chosen directory
                    let dirPat = hasVersion "css" .&&. fromGlob (dirName ++ "/*")
                    makeItem . compressCss . concatMap itemBody =<< loadAll dirPat


fullSite :: HasCallStack => Rules ()
fullSite = do
    match "templates/**" $ compile templateCompiler

    specialFiles
    copiedFiles
    buildStylesheets
