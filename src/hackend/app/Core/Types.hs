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
type TopicCode = Code
type TraitCode = Code
type LessonCode = Code
-- type SupplementCode = File
type TaskCode = Code
type Title = String
type Description = String
type Name = String
type Author = String
type Content = String
type File = String
type Difficulty = Int
type Time = Int

data Topic = Topic TopicCode Title [LessonCode] [TraitCode] Description
  deriving (Show, Generic)

instance FromJSON Topic
  where
    parseJSON (Object v) = Topic <$>
                           v .: "code" <*>
                           v .: "title" <*>
                           v .: "lessons" <*>
                           v .: "traits" <*>
                           v .: "description"
    parseJSON _ = mzero

instance ToJSON Topic
  where
    toJSON (Topic code title lessons traits description) = object
      [
        "code" .= code,
        "title" .= title,
        "lessons" .= lessons,
        "traits" .= traits,
        "description" .= description
      ]

data Trait = Trait TraitCode Name Description
  deriving (Show, Generic)

instance FromJSON Trait
  where
    parseJSON (Object v) = Trait <$>
                           v .: "code" <*>
                           v .: "name" <*>
                           v .: "description"
    parseJSON _ = mzero

instance ToJSON Trait
  where
    toJSON (Trait code name description) = object
      [
        "code" .= code,
        "name" .= name,
        "description" .= description
      ]

data Lesson = Lesson LessonCode Title Content [TraitCode] [Supplement] [TaskCode] Author
  deriving (Show, Generic)

instance FromJSON Lesson
  where
    parseJSON (Object v) = Lesson <$>
                           v .: "code" <*>
                           v .: "title" <*>
                           v .: "content" <*>
                           v .: "traits" <*>
                           v .: "supplement" <*>
                           v .: "tasks" <*>
                           v .: "author"
    parseJSON _ = mzero

instance ToJSON Lesson
  where
    toJSON (Lesson code title content traits supplements tasks author) = object
      [
        "code" .= code,
        "title" .= title,
        "content" .= content,
        "traits" .= traits,
        "supplement" .= supplements,
        "tasks" .= tasks,
        "author" .= author
      ]

data Supplement = Supplement Title File
  deriving (Show, Generic, ToJSON)

instance FromJSON Supplement
  where
    parseJSON (Object v) = Supplement <$>
                           v .: "title" <*>
                           v .: "file"
    parseJSON _ = mzero

data Task = Task TaskCode Title Content [Supplement] Difficulty Time
  deriving (Show, Generic, ToJSON)

instance FromJSON Task
  where
    parseJSON (Object v) = Task <$>
                           v .: "title" <*>
                           v .: "code" <*>
                           v .: "content" <*>
                           v .: "supplement" <*>
                           v .: "difficulty" <*>
                           v .: "time"
    parseJSON _ = mzero
