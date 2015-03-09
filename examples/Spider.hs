-- Spider a website (over HTTP) and print all URLs visited
-- Uses stateful queues to avoid repeat URLs
module Main where

import qualified Data.Set.Monad as Set
import qualified Network.HTTP as HTTP
import qualified Network.Stream as Net
import Control.Monad.State
import System.Environment
import Text.Regex.Posix
import Data.Maybe
import Data.List.Ordered
import TaskQueue
import HTTPWorker

type SpiderResult = Result [String]
type SpiderResponse = Response [String]

-- This monstrosity is to keep track of the number of tasks assigned, and the URLs which have been visited
type SpiderQueue = StateT (Int, Set.Set String) IO (HTTPQueue [String])

isNewResult :: SpiderResult -> Bool
isNewResult (Result x _) = x == 0

targetURLs :: SpiderResult -> [String]
targetURLs (Result _ y) = y

requestBuilder :: String -> Task -> [SpiderResult] -> HTTP.Request_String
requestBuilder defaultURL task results = HTTP.getRequest $ if dependencyIDs task == []
	then defaultURL
	else head $ targetURLs $ head $ results

extractHTTPLinks :: String -> [String]
extractHTTPLinks "" = []
extractHTTPLinks str = (\(x,y,z) -> if y == ""
						then []
						else (init $ tail y) : extractHTTPLinks z) match 
	where
		match = str =~ "\"http://[^\"]+\"" :: (String, String, String)

responseParser :: Task -> Net.Result (HTTP.Response String) -> IO SpiderResponse
responseParser task result = do
	code <- HTTP.getResponseCode result
	if code /= (2,0,0)
		then return $ Response Nothing []	-- Don't retry on errors
		else do
			body <- HTTP.getResponseBody result
			let newURLs = extractHTTPLinks body
			let newResult = Result 0 newURLs
			return $ Response (Just newResult) []

-- duplicate URLs are why we need to keep track of state
removeDuplicateURLs :: SpiderQueue -> SpiderQueue
removeDuplicateURLs spiderQueue = do
	queue <- spiderQueue
	(baseTaskID, visitedURLs) <- get

	-- get the results which were just received
	let results = getResults queue
	let newResults = filter isNewResult results
	let oldResults = filter (not . isNewResult) results

	-- flatten the newResults and remove the ones which have already been visited
	let newURLs = nub $ sort $ filter (not . flip Set.member visitedURLs) $ concat $ map targetURLs newResults
	let newURLCount = length newURLs
	let newResults = map (\n -> Result (baseTaskID + n) [newURLs !! (n - 1)]) [1..newURLCount]

	-- only output of program, besides errors
	liftIO $ sequence $ map putStrLn newURLs

	-- In order to avoid repeat visits, the queue needs to take over dispatching tasks
	let newTasks = map (\n -> Task (baseTaskID + n) [baseTaskID + n]) [1..newURLCount]

	-- update state
	let allVisitedURLs = Set.union visitedURLs $ Set.fromList newURLs
	put (baseTaskID + newURLCount, allVisitedURLs)

	return $ Queue (readyWorkers queue) (busyWorkers queue) (allTasks queue ++ newTasks) (oldResults ++ newResults)

main :: IO ()
main = do
	args <- getArgs
	let numWorkers = read $ head args
	let timeout = read (args !! 1)
	let firstURL = args !! 2
	workers <- sequence $ map (\_ -> newHTTPWorker (requestBuilder firstURL) responseParser timeout) [0 .. numWorkers - 1]
	let queue = newQueue workers [Task 0 []]
	let statefulQueue = liftIO $ return queue	:: SpiderQueue
	execStateT (runWithState statefulQueue removeDuplicateURLs) (1, Set.fromList [firstURL]) >> return ()
