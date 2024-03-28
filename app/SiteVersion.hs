module SiteVersion where

import Data.Char (isSpace)
import Data.List (dropWhileEnd, intercalate)

import GHC.Stack (HasCallStack)

import System.Environment (lookupEnv)
import System.Process (readProcess)

lookupSiteVersion :: HasCallStack => IO String
lookupSiteVersion = do
    gsu <- lookupEnv "GITHUB_SERVER_URL"
    gr <- lookupEnv "GITHUB_REPOSITORY"
    let repoUrl = liftA2 (\s p -> s ++ "/" ++ p) gsu gr
        repoPart = foldMap (\x -> [("repository", x)]) repoUrl

    let rp c p i = trim <$> readProcess c p i
        trim = dropWhile isSpace . dropWhileEnd isSpace

    gitInfo <- rp "git" ["show", "-s", "--format=%H %ci"] ""
    let gitHashPart = [("commit", takeWhile (/= ' ') gitInfo)]
        gitDatePart = [("date", drop 1 . dropWhile (/= ' ') $ gitInfo)]

    ghcVersion <- rp "ghc" ["--numeric-version"] ""
    cabalVersion <- rp "cabal" ["--numeric-version"] ""
    let ghcPart = [("ghc-version", ghcVersion)]
        cabalPart = [("cabal-version", cabalVersion)]

    -- fragile; only works because no content needs escaping
    let toJSON ls = "{" ++ intercalate "," (map pairToEntry (concat ls)) ++ "}"
        pairToEntry (k, v) = "\"" ++ k ++ "\":\"" ++ v ++ "\""
    return $ toJSON [ repoPart, gitHashPart, gitDatePart, ghcPart, cabalPart ]
