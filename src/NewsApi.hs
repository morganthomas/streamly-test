{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}

module NewsApi where

import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as C8
import           Data.Maybe (fromMaybe)
import           Data.Semigroup
import           Data.Text (Text)
import qualified Data.Text as T
import           Network.HTTP.Conduit
import           Network.URI
import           Text.Ascii (ascii)

newtype StoryId = StoryId Int

instance FromJSON StoryId where
  parseJSON x = StoryId <$> parseJSON x

newtype CommentId = CommentId Int

instance FromJSON CommentId where
  parseJSON x = CommentId <$> parseJSON x

newtype UserId = UserId Text deriving (Eq, Ord)

instance FromJSON UserId where
  parseJSON x = UserId <$> parseJSON x

data Story c = Story { storyId :: StoryId, storyTitle :: Text, comments :: [c] }

instance FromJSON (Story CommentId) where
  parseJSON = withObject "Story" $ \o -> do
    id <- o .: "id"
    nm <- o .: "title"
    cs <- fromMaybe [] <$> o .:? "kids"
    return (Story id nm cs)

data Comment = Comment { commentId :: CommentId, commentBy :: UserId }

instance FromJSON Comment where
  parseJSON = withObject "Comment" $ \o -> do
    id <- o .: "id"
    by <- o .: "by"
    return (Comment id by)

hackerNewsApiBase :: String
hackerNewsApiBase = "https://hacker-news.firebaseio.com/v0/"

storiesApiUrl :: String
storiesApiUrl = hackerNewsApiBase <> "topstories.json"

topStories :: IO [StoryId]
topStories = do
  res <- simpleHttp storiesApiUrl
  case decode res of
    Just stories -> return stories
    Nothing -> error ("getStories: could not parse response: " <> C8.unpack res)

storyApiUrl :: StoryId -> String
storyApiUrl (StoryId id) = hackerNewsApiBase <> "item/" <> show id <> ".json"

getStory :: StoryId -> IO (Story CommentId)
getStory id = do
  res <- simpleHttp (storyApiUrl id)
  case decode res of
    Just story -> return story
    Nothing -> error ("getStory: could not parse response: " <> C8.unpack res)

commentApiUrl :: CommentId -> String
commentApiUrl (CommentId id) = hackerNewsApiBase <> "item/" <> show id <> ".json"

-- getComment may fail to return a comment because the comment may be deleted in which case
-- it will not parse because there will be no "by" property
getComment :: CommentId -> IO (Maybe Comment)
getComment id = do
  res <- simpleHttp (commentApiUrl id)
  return (decode res) 
