module Core.Types
  where

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
  deriving (Show)

data Trait = Trait Code Name Description
  deriving (Show)

data Lesson = Lesson Code Title Content [Trait] [Supplement] [Task] Author
  deriving (Show)

data Supplement = Supplement Title File
  deriving (Show)

data Task = Task Code Title Content [Supplement] Difficulty Time
  deriving (Show)