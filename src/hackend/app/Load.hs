module Load (loadState)
  where

import Core.Types (State (State), Topic, Trait, Lesson, Task, Supplement)

import Control.Monad (filterM)
import Data.Aeson
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe, maybeToList)
import qualified Data.ByteString.Lazy as LB
import qualified System.Directory as SD

loadState :: FilePath -> IO State
loadState d = do
    topics      <- (findFilesWith (return . isPrefixOf "topic")      d >>= loads) :: IO [Topic]
    traits      <- (findFilesWith (return . isPrefixOf "trait")      d >>= loads) :: IO [Trait]
    lessons     <- (findFilesWith (return . isPrefixOf "lesson")     d >>= loads) :: IO [Lesson]
    tasks       <- (findFilesWith (return . isPrefixOf "task")       d >>= loads) :: IO [Task]
    supplements <- (findFilesWith (return . isPrefixOf "supplement") d >>= loads) :: IO [Supplement]
    return $ State topics traits lessons tasks supplements

loads :: (FromJSON a) => [FilePath] -> IO [a]
loads fs = mapM load fs >>= return . concat

load :: (FromJSON a) => FilePath -> IO [a]
load file = do
    content <- LB.readFile file
    let x = decode content -- :: Maybe a
    let xs = decode content -- :: Maybe [a]
    return $ fromMaybes x xs
  where
    fromMaybes :: Maybe a -> Maybe [a] -> [a]
    fromMaybes x = fromMaybe (maybeToList x)

findFilesWith :: (FilePath -> IO Bool) -> FilePath -> IO [FilePath]
findFilesWith p dir = findFilesWith' dir >>= filterM p
  where
    findFilesWith' :: FilePath -> IO [FilePath]
    findFilesWith' dir' = do
        files <- SD.listDirectory dir' >>= filterM SD.doesFileExist
        dirs <- SD.listDirectory dir' >>= filterM SD.doesDirectoryExist
        files' <- mapM findFilesWith' dirs >>= return . concat
        return $ files ++ files'