-- | chapter 9 real-life-hakell

module Ch9 where

import Control.Monad (forM, filterM)
import qualified Control.Exception as E
import System.IO (IOMode(..), hClose, hFileSize, openFile)
import Data.Time.Clock
import System.Directory(
  Permissions(..),
 getModificationTime,
 getPermissions,
  doesDirectoryExist,
  getDirectoryContents  -- FilePath -> IO [FilePath]
  )
import System.FilePath ( (</>), takeExtension )


getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents topdir = do
  names <- getDirectoryContents topdir
  let properNames = filter (`notElem` [".", ".."]) names
  paths <- forM properNames $ \name -> do
    let path = topdir </> name
    isDirectory <- doesDirectoryExist path
    if isDirectory
      then getRecursiveContents path
      else return [path]
  return ( concat paths )

simpleFind :: (FilePath -> Bool) -> FilePath -> IO [FilePath]
simpleFind p path =  do
  names <- getRecursiveContents path
  return ( filter p names )

type Predicate =  FilePath -> Permissions -> Maybe Integer -> UTCTime -> Bool
    
getFileSize :: FilePath -> IO (Maybe Integer)
getFileSize path =  E.handle (\_ -> return Nothing) $ do
  h <- openFile path ReadMode
  size <- hFileSize h
  hClose h
  return (Just size)

betterFind :: Predicate -> FilePath -> IO [FilePath]
betterFind p path =  getRecursiveContents path >>= filterM check where
  check name = do
    perms <- getPermissions name
    size <- getFileSize name
    modified <- getModificationTime name
    return ( p name perms size modified )
