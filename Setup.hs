import Distribution.Simple

import Distribution.Simple.Utils(warn)
import Distribution.Verbosity(normal)
import System.FilePath(getSearchPath,(</>))
import System.Directory(doesFileExist,getPermissions,executable)
import Control.Monad(filterM,unless)

main = do
  let hooks = simpleUserHooks {
        preConf = \args cnfFlags -> do
           info <- (preConf simpleUserHooks) args cnfFlags
           checkPHC
           return info
        }
  defaultMainWithHooks hooks

checkPHC :: IO ()
checkPHC = do
  locations <- phcLocations
  (if null locations then 
     warn normal "No PHC binary found in PATH."
   else do
     p <- mapM getPermissions locations
     unless (any executable p) $ warn normal "PHC file found, but not executable.")

-- TODO: allow user to specify location and name of "phc" binary
phcLocations :: IO [FilePath]
phcLocations = do
  paths <- getSearchPath
  filterM doesFileExist $ map (</> "phc") paths
  
