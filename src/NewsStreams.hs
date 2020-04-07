module NewsStreams where

import           Control.Concurrent (forkIO)
import           Control.Concurrent.MVar
import           Control.Monad (forM)
import           Data.Maybe (catMaybes)
import           Streamly
import qualified Streamly.Prelude as S

import           NewsApi

getStories :: [StoryId] -> AsyncT IO (Story CommentId)
getStories ids = parallely . S.fromFoldableM $ getStory <$> ids
 
getStoryComments :: Story CommentId -> AsyncT IO (Maybe Comment)
getStoryComments story = parallely . S.fromFoldableM $ getComment <$> comments story

getStoriesWithComments :: AsyncT IO (Story CommentId) -> AsyncT IO (Story Comment)
getStoriesWithComments = S.mapM $ \story -> do
  cs <- S.toList . adapt $ getStoryComments story
  return (Story (storyId story) (storyTitle story) (catMaybes cs))
