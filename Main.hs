{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- Pairing exercise
-- =================
--
-- This is not a technical exercise. Feel free to ask questions and use reference materials
-- (Google, StackOverflow, Hackage, etc.).
--
-- This is a chance for everyone to see how everyone else works together.
--
--
-- Exercise:
-- ----------
--
-- Implement `deserializeUser` and `serializeUser` to deserialize and serialize
-- a Haskell value without using `aeson`'s type classes.
--
-- When running `main`, the output should be
--
--     userResult == sampleUser: True
--     jsonResult == sampleJson: True
--     Done
--
-- A `User` has a `Name` and a list of `BlogPost`s.
-- A `BlogPost` has a `Body`.
--
--

module Main where

import Control.Applicative (pure, (<$>), (<*>))
import Data.Aeson (object)
import Data.Aeson.QQ (aesonQQ)
import Data.Aeson.Types (Parser, Value(..), parseMaybe)
import Data.HashMap.Strict ((!))
import Data.Text (Text)
import Data.Traversable (sequenceA)
import Data.Vector (fromList, toList)
import GHC.Stack (errorWithStackTrace)


-- User
newtype Name = Name { unName :: Text } deriving (Eq, Show)

data User = User { userName :: !Name
                 , userBlogPosts :: ![BlogPost]
                 } deriving (Eq, Show)

-- BlogPost
newtype Body = Body { unBody :: Text } deriving (Eq, Show)

data BlogPost = BlogPost { blogPostBody :: !Body} deriving (Eq, Show)


-- General notes
-- --------------
--
-- data Value = Object !Object
--            | Array !Array
--            | String !Text
--            | Number !Scientific
--            | Bool !Bool
--            | Null
--              deriving (Eq, Show, Typeable, Data)
--
-- type Object = HashMap Text Value
-- type Array = Vector Value
--

-- Deserialization
-- ----------------
--
-- class FromJSON a where
--   parseJSON :: Value -> Parser a
--
-- parseMaybe :: (a -> Parser b) -> a -> Maybe b
--

-- Take in an `aeson` `Value` and return a `Maybe User`
deserializeUser :: Value -> Maybe User
deserializeUser jsonValue = undefined


-- Serialization
-- --------------
--
-- class ToJSON a where
--   toJSON :: a -> Value
--
-- object :: [Pair] -> Value
-- type Pair = (Text, Value)
--

-- Take in a `User` and return an `aeson` `Value`.
serializeUser :: User -> Value
serializeUser u = undefined


-- Sample data
-- ------------


sampleUser :: User
sampleUser = User (Name "Sarah") samplePosts

samplePosts :: [BlogPost]
samplePosts = [ BlogPost (Body "This post is so awesome!")
              , BlogPost (Body "This post is so also awesome!")
              , BlogPost (Body "This post is even better!")
              ]

sampleUserJsonValue :: Value
sampleUserJsonValue = [aesonQQ|
  {
    "name": "Sarah",
    "blogPosts": [
      {
        "body": "This post is so awesome!"
      },
      {
        "body": "This post is so also awesome!"
      },
      {
        "body": "This post is even better!"
      }
    ]
  }
|]


-- main
-- -----

main :: IO ()
main = do
  putStrLn $ "userResult == sampleUser: " ++ show (userResult == Just sampleUser)
  putStrLn $ "jsonResult == sampleJson: " ++ show (jsonResult == sampleUserJsonValue)
  putStrLn "Done"
  where userResult = deserializeUser sampleUserJsonValue
        jsonResult = serializeUser sampleUser

