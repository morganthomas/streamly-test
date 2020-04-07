module Main where

import           Control.Monad (forM)
import           Data.List (sortBy)
import           Data.Map (Map)
import qualified Data.Map as M
import           Streamly
import qualified Streamly.Prelude as S
import qualified Data.Text as T

import           NewsApi
import           NewsStreams

maxNumStories :: Int
maxNumStories = 30

numCommenters :: Int
numCommenters = 10

getTopCommenters :: [Story Comment] -> [(UserId, Int)]
getTopCommenters stories =
  let commenters :: Map UserId Int
      commenters = foldl addCommenters mempty stories

      addCommenters :: Map UserId Int -> Story Comment -> Map UserId Int
      addCommenters cs story = foldl addCommenter cs (comments story)

      addCommenter :: Map UserId Int -> Comment -> Map UserId Int
      addCommenter cs (Comment _ by) = M.unionWith (+) cs (M.singleton by 1) in

    take numCommenters . sortBy (\(_, i) (_, j) -> compare j i) $ M.toList commenters

printStory :: Story c -> IO (Story c)
printStory s = do
  putStrLn (T.unpack (storyTitle s))
  return s

main :: IO ()
main = do
  putStrLn "TOP STORIES"
  storyIds <- take maxNumStories <$> topStories
  let numStories = length storyIds
  stories <- S.toList . adapt . S.take numStories
             . getStoriesWithComments . S.mapM printStory . S.take numStories
             $ getStories storyIds
  putStrLn "\nTOP COMMENTERS"
  forM (getTopCommenters stories) $
    \(UserId name, count) -> putStrLn (T.unpack name <> " - " <> show count)
  putStrLn "\nDONE"
