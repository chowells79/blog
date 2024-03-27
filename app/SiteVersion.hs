module SiteVersion where

import System.Environment (lookupEnv)
import System.Process (readProcess)

lookupSiteVersion :: IO String
lookupSiteVersion = do
    gsu <- lookupEnv "GITHUB_SERVER_URL"
    gr <- lookupEnv "GITHUB_REPOSITORY"
    let repoUrl = liftA2 (\s p -> s ++ "/" ++ p) gsu gr
        repoPair = foldMap (\x -> [("respository", x)]) repoUrl

    gitInfo <- readProcess "git" ["show", "-s", "--format=%H %ci"] ""
    return ""
