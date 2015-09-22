{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
import qualified Data.ByteString.Char8 as S8
import           Data.Monoid           ((<>))
import           IdeSession
-- From Cabal-ide-backend
-- for parsing the cabal file and extracting lang extensions used.
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Distribution.PackageDescription.Configuration
import Language.Haskell.Extension

import "Glob" System.FilePath.Glob (glob, globDir, Pattern, compile)

import Control.Monad.STM
import Control.Concurrent.STM.TVar

import System.FSNotify
import System.FSNotify.Devel
import Control.Monad      (forever)

import Control.Concurrent (forkIO, killThread, threadDelay, ThreadId)

import System.Process (readProcessWithExitCode)
import System.Environment (unsetEnv, setEnv)
import System.Exit (ExitCode(..))

-- Paths
import Data.List ((\\))
import System.FilePath ((</>))
import System.Directory (getCurrentDirectory, doesDirectoryExist, getDirectoryContents)
import Control.Monad (forM)
import Control.Concurrent (forkIO, killThread, threadDelay)
import System.FilePath.Posix (replaceExtension, dropExtension, takeExtensions)
import Control.Monad.IO.Class (liftIO)
import System.FilePath (pathSeparator)
import System.Directory (removeFile)
import System.FilePath (pathSeparator)
import Data.Text (unpack)
import Data.String.Utils
import System.Directory (getCurrentDirectory)
import qualified Data.ByteString.Char8 as C8


compile' :: Maybe IdeSession -> Bool -> IO IdeSession
compile' mSession isRebuild = do

  (sess, upd) <- if isRebuild
                      then 
                        case mSession of
                          Just sess -> return (sess, mempty)
                          Nothing -> fail "Session not found."
                      else do
                        -- Initialization and providing some code
                        conf <- sessionConfigFromEnv
                        sess <- initSession defaultSessionInitParams conf

                        extensionList <- getExtensions

                        let update = updateTargets (TargetsInclude ["Application.hs"])
                                     <> updateCodeGeneration True
                                     <> updateGhcOpts (["-ddump-hi", "-ddump-to-file"] ++ ["-Wall"] ++ extensionList)
                        return (sess, update)
  updateSession sess upd print -- print is used for progress updates

  -- Print errors and warnings
  errs <- getSourceErrors sess
  mapM_ print errs
  return sess

main :: IO ()
main = setConfig >> build Nothing False

build :: Maybe IdeSession -> Bool -> IO ()
build mSession isRebuild = do
  sess <- compile' mSession isRebuild
  (ra, tid) <- run sess
  
  -- Start watching for file changes.
  isDirty <- newTVarIO False
  

  -- Watch for changes in the current working directory.
  watchId <- forkIO $ watch isDirty sess

  -- Block until relevant change is made then carry on with program execution.
  checkForChange isDirty
  
  -- restart all
  killThread watchId
  killThread tid
  interrupt ra
  
  putStrLn "\n\nRebuilding...\n\n"
  
  build (Just sess) True

run :: IdeSession -> IO (RunActions RunResult, ThreadId)
run sess = do
  ra <- runStmt sess "Application" "develMain"
  tid <- forkIO $ loop ra
  return (ra, tid)

-- | Run for as long as we need to.
loop :: RunActions RunResult -> IO ()
loop res = do
  runAction <- runWait res
  case runAction of
    Left bs -> S8.putStr bs >> loop res
    Right result -> putStrLn $ "Run result: " ++ show result


{- ------------------------------------------------------------------
Ignore the following helper functions.
--------------------------------------------------------------------- -}

--  ------------------- File ext ------------
-- | Parse the cabal file to get the ghc extensions in use.
getExtensions :: IO [String]
getExtensions = do 

  let getCabalFile :: IO FilePath
      getCabalFile = do
        list <- glob "*cabal"
        case list of
          [] -> fail "No cabal file."
          (cabalFile:_) -> return cabalFile              

  cabalFilePath <- getCabalFile
  cabalFile <- readFile cabalFilePath

  let unsafePackageDescription = parsePackageDescription cabalFile

      genericPackageDescription = case unsafePackageDescription of
                                ParseOk _ a -> a
                                _           -> error "failed package description."

      packDescription = flattenPackageDescription genericPackageDescription

  let rawExt = usedExtensions $ head $ allBuildInfo packDescription
      parseExtension :: Extension -> String
      parseExtension (EnableExtension extension) =  "-X" ++ show extension
      parseExtension (DisableExtension extension) = "-XNo" ++ show extension
      parseExtension (UnknownExtension extension) = "-X" ++ show extension

      extensions = map parseExtension rawExt
  return extensions

--  ------------------- File watching ------------

-- Watches for file changes for the specified file extenstions
-- When a change is found. It modifies isDirty to True.
-- "Smart" file watching.
watch :: TVar Bool -> IdeSession -> IO ()
watch isDirty session = do
  pathsToWatch <- getFilesToWatch session
  manager <- startManagerConf defaultConfig
  _ <- watchTree manager "." (const True)
         -- Last argument to watchTree.
         (\event -> do
            let getFilePath :: Event -> FilePath
                getFilePath (Added fp _)    = fp
                getFilePath (Modified fp _) = fp
                getFilePath (Removed fp _)  = fp

                isModified = getFilePath event `elem` pathsToWatch
            threadDelay 1000
            atomically $ writeTVar isDirty isModified)

  _ <- forever $ threadDelay maxBound
  stopManager manager

checkForChange :: TVar Bool -> IO ()
checkForChange isDirty =
  atomically $ do readTVar isDirty >>= check
                  writeTVar isDirty False


setConfig :: IO ()
setConfig = do
  (path, pkgDb) <- getConfig
  _ <- unsetEnv "PATH"
  _ <- unsetEnv "GHC_PACKAGE_PATH"
  _ <- setEnv "PATH" path
  setEnv "GHC_PACKAGE_PATH" pkgDb


type PATH = String
type PACKAGEDB = String
type Config = (PATH, PACKAGEDB)

getConfig :: IO Config
getConfig = do
  (exitCode, stdout, stderr) <- readProcessWithExitCode "stack" ["path"] ""
  case exitCode of
    ExitSuccess   -> parseConfig stdout
    ExitFailure _ -> fail stderr
  where 
    parseConfig :: String -> IO Config
    parseConfig stdout = do
      let outputList = lines stdout
          tupleList = map (span (/=':') ) outputList
          path = concatMap getPath tupleList
          pkgDb = concatMap getPkgDb tupleList ++ ":"
      return (path, pkgDb)


getPath :: (String, String) -> String
getPath (key,value)
        | key == "bin-path" = dropWhile (==':') $ filter (/=' ') value
        | otherwise = ""

getPkgDb :: (String, String) -> String
getPkgDb (key,value)
          | key == "snapshot-pkg-db" = dropWhile (==':') $ filter (/=' ') value
          | otherwise = ""



{- ------------------------ File watching stuff -------------------------- -}
getFilesToWatch :: IdeSession -> IO [FilePath]
getFilesToWatch session = do
  dir <- getCurrentDirectory
  srcPaths <- getPathsForCompiledFiles session
  let replaceWithHiDump :: FilePath -> FilePath
      replaceWithHiDump srcFile =  replaceExtension  srcFile ".dump-hi"
      withHiDumpExt = map replaceWithHiDump srcPaths
  thDeps' <- mapM parseHi withHiDumpExt
  -- Removing quotes.
  mixedThDeps  <- return $ map (takeWhile (/='\"') . dropWhile (=='\"') . dropWhile (/='\"')) $ concat thDeps'
  -- make rel paths absolute and leave the absolute ones intact
  -- mixedThDeps meaning there are both absolute and relative paths here.
  let makePathsAbsolute :: FilePath -> FilePath
      makePathsAbsolute [] = dir ++ [pathSeparator]
      makePathsAbsolute fp@(x:_)
        | x == pathSeparator = fp
        | otherwise = dir ++ [pathSeparator] ++ fp
      thDeps = map makePathsAbsolute mixedThDeps

  -- Add the cabal file path to paths to watch for.
  cabalFile <- getCabalFile
  -- Clean up after GHC
  -- delitterId <- forkIO $ delitter
  -- threadDelay 1000
  -- killThread delitterId
  return $ cabalFile : srcPaths ++ thDeps 

getCabalFile :: IO FilePath
getCabalFile = do
  list <- glob "*cabal"
  case list of
    [] -> fail "No cabal file."
    (cabalFile:_) -> return cabalFile

parseHi :: FilePath -> IO [FilePath]
parseHi path = do
  dumpHI <- liftIO $ fmap C8.lines (C8.readFile path)
  let thDeps' =
          -- The dependent file path is surrounded by quotes but is not escaped.
          -- It can be an absolute or relative path.
          filter ("addDependentFile \"" `C8.isPrefixOf`) dumpHI
  return $ map C8.unpack thDeps'

-- | Match the possible compiled files agaist the actual source files.
-- Gives you the actual source files.
getPathsForCompiledFiles :: IdeSession -> IO [FilePath]
getPathsForCompiledFiles session = do
  compiledNoExt <- getCompiledFiles session
  allSourceFiles <- getManagedFiles' -- With extensions
  return $ matchCompiledFiles compiledNoExt allSourceFiles
  where matchCompiledFiles :: [FilePath] -> [FilePath] -> [FilePath]
        matchCompiledFiles _ [] = []
        matchCompiledFiles [] _ = []
        matchCompiledFiles list@(x:xs) (y:ys)
          | x == (dropExtension y) = y : matchCompiledFiles xs ys
          | otherwise = matchCompiledFiles list (ys ++ [y])

-- | These are the source and data files.
-- Not related or to be confused with ide-backend's
-- getManagedFiles
getManagedFiles' :: IO [FilePath]
getManagedFiles' =  do
  cwd <- getCurrentDirectory
  getRecursiveContents cwd

-- | Get the files that actually exist in the given dir.
-- In this case it's called with the source dirs
getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents topdir = do
  names <- getDirectoryContents topdir

  -- We want to take these files out.
  let patterns :: [Pattern]
      patterns = [ (compile "*.*~")
                     , (compile "*.hi")
                     , (compile "*.dump-hi")
                     , (compile "*.o")
                     , (compile "*.dyn_o")
                     , (compile "*.dyn_hi")
                     , (compile "*.so")
                     , (compile "*.conf")
                     , (compile "*.h")
                     , (compile "*.a")
                     , (compile "*.inplace")
                     , (compile "*.cache")
                     , (compile "*.*.el")
                     ]
  (x, _) <- globDir patterns topdir

  let properNames = filter (`notElem` [".", ".."]) names
  paths <- forM properNames $ \name -> do
   let path = topdir </> name
   isDirectory <- doesDirectoryExist path
   if isDirectory
     then getRecursiveContents path
     else return $ [path] \\ (concat x)
  return (concat paths)


-- | Paths without extensions.
-- Uncomment for absolute paths.
getCompiledFiles :: IdeSession -> IO [FilePath]
getCompiledFiles session = do

  moduleList <- getLoadedModules session
  dir <- getCurrentDirectory

  let toFilePath :: ModuleName -> FilePath -- A.B.C = A/B/C
      toFilePath moduleName' = replace "." [pathSeparator] $ unpack moduleName'
      absPaths = map (((dir ++ "/") ++) . toFilePath) moduleList

  -- We use absolute paths because when we match them against dir contents we use absolute paths.
  -- or the result of Devel.Paths.getRecursiveContents
  return absPaths
