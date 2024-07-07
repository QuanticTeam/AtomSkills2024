{-# LANGUAGE OverloadedStrings #-}

module Load (loadState)
  where

import Core.Types (State (State), Trait)

import Control.Monad (guard, filterM)
import Data.Aeson
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe)
import qualified Data.ByteString.Lazy as LB
import System.Directory as SD

loadState :: String -> IO State
loadState d = do
    fs <- listDirectory d
    -- xxx <- listDirectory "."
    -- meow <- getBinDir
    -- print "DT!"
    -- print $ show xxx
    -- _ <- error $ show xxx
    traits <- loadTraits "./lessons/traits.json"
    return $ State undefined traits undefined undefined undefined

loadTraits :: String -> IO [Trait]
loadTraits f = do
    _ <- guardFileName "trait" f
    content <- LB.readFile f
    let traits = (decode content) :: Maybe [Trait]
    return $ fromMaybe [] traits

guardFileName :: String -> FilePath -> IO ()
guardFileName fileName = guard . isPrefixOf fileName

findFilesWith :: (FilePath -> IO Bool) -> [FilePath] -> IO [FilePath]
findFilesWith p dirs = undefined

listDirectoryR :: FilePath -> IO [FilePath]
listDirectoryR dir = do
    files <- SD.listDirectory dir >>= filterM SD.doesFileExist
    dirs <- SD.listDirectory dir >>= filterM SD.doesDirectoryExist
    files' <- mapM listDirectoryR dirs >>= return . concat
    return $ files ++ files'