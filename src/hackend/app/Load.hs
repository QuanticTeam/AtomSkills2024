module Load (loadState)
  where

import Core.Types (State (State), Topic, Trait, Lesson, Task, Supplement)

import Control.Monad (filterM)
import Data.Aeson
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe, maybeToList)
import qualified Data.ByteString.Lazy as LB
import qualified System.Directory as SD
import System.FilePath ((</>), isExtensionOf, takeFileName)

loadState :: FilePath -> IO State
loadState d = do
    topics      <- (findFilesWith (return . isPrefixOf "topic" . takeFileName)      d >>= loads) :: IO [Topic]
    traits      <- (findFilesWith (return . isPrefixOf "trait" . takeFileName)      d >>= loads) :: IO [Trait]
    lessons     <- (findFilesWith (return . isPrefixOf "lsn" . takeFileName)     d >>= loads) :: IO [Lesson]
    tasks       <- (findFilesWith (return . isPrefixOf "tsk" . takeFileName)       d >>= loads) :: IO [Task]
    supplements <- return [] --(findFilesWith (return . isPrefixOf "supplement" . takeFileName) d >>= loads) :: IO [Supplement]
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
        entries <- SD.listDirectory dir' >>= mapM (return . (dir' </>))
        files <- filterM SD.doesFileExist entries
        dirs <- filterM SD.doesDirectoryExist entries
        files' <- mapM findFilesWith' dirs >>= return . concat
        return $ files ++ files'