module NewsStreams where

import           Control.Concurrent (forkIO)
import           Control.Concurrent.MVar
import           Control.Monad (forM)
import           Data.Maybe (catMaybes)
import           Streamly
import qualified Streamly.Prelude as S

import           NewsApi

getStories :: [StoryId] -> IO (AsyncT IO (Story CommentId))
getStories ids = do
  push <- newEmptyMVar
  forM ids $ \id -> forkIO $ do
    story <- getStory id
    putMVar push story
  return (S.repeatM (takeMVar push))
 
getStoryComments :: Story CommentId -> IO (AsyncT IO (Maybe Comment))
getStoryComments story = do
  push <- newEmptyMVar
  forM (comments story) $ \id -> forkIO $ do
    comment <- getComment id
    putMVar push comment
  return (S.repeatM (takeMVar push))

getStoriesWithComments :: AsyncT IO (Story CommentId) -> IO (AsyncT IO (Story Comment))
getStoriesWithComments stories = do
  push <- newEmptyMVar
  flip S.mapM_ (adapt stories) $ \story -> forkIO $ do
    cs <- S.toList . adapt . S.take (length (comments story)) =<< getStoryComments story
    putMVar push (Story (storyId story) (storyTitle story) (catMaybes cs))
  return (S.repeatM (takeMVar push))
