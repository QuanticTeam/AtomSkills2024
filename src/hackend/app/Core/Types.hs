{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Core.Types
  where

import Control.Monad (mzero)
import Data.Aeson
import GHC.Generics

data State = State {
  topics :: [Topic],
  traits :: [Trait],
  lessons :: [Lesson],
  tasks :: [Task],
  supplements :: [Supplement]
} deriving (Generic, ToJSON)

type Code = String
type Title = String
type Description = String
type Name = String
type Author = String
type Content = String
type File = String
type Difficulty = Int
type Time = Int

data Topic = Topic Code Title [Lesson] [Trait] Description
  deriving (Show, Generic, ToJSON)

instance FromJSON Topic
  where
    parseJSON (Object v) = Topic <$>
                           v .: "code" <*>
                           v .: "title" <*>
                           v .: "lessons" <*>
                           v .: "traits" <*>
                           v .: "description"
    parseJSON _ = mzero

data Trait = Trait Code Name Description
  deriving (Show, Generic, ToJSON)

instance FromJSON Trait
  where
    parseJSON (Object v) = Trait <$>
                           v .: "code" <*>
                           v .: "name" <*>
                           v .: "description"
    parseJSON _ = mzero

data Lesson = Lesson Code Title Content [Trait] [Supplement] [Task] Author
  deriving (Show, Generic, ToJSON)

instance FromJSON Lesson
  where
    parseJSON (Object v) = Lesson <$>
                           v .: "code" <*>
                           v .: "title" <*>
                           v .: "content" <*>
                           v .: "traits" <*>
                           v .: "supplements" <*>
                           v .: "tasks" <*>
                           v .: "author"
    parseJSON _ = mzero

data Supplement = Supplement Title File
  deriving (Show, Generic, ToJSON)

instance FromJSON Supplement
  where
    parseJSON (Object v) = Supplement <$>
                           v .: "title" <*>
                           v .: "file"
    parseJSON _ = mzero

data Task = Task Code Title Content [Supplement] Difficulty Time
  deriving (Show, Generic, ToJSON)

instance FromJSON Task
  where
    parseJSON (Object v) = Task <$>
                           v .: "title" <*>
                           v .: "code" <*>
                           v .: "content" <*>
                           v .: "supplements" <*>
                           v .: "difficulty" <*>
                           v .: "time"
    parseJSON _ = mzero
