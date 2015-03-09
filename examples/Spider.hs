-- Spider a website (over HTTP) and print all URLs visited
-- Does not check for repeated links, but won't use the same result multiple times
-- I.E. if the website graph is acyclic, no links will be repeated
module Main where

import System.Environment
import qualified Network.HTTP as HTTP
import qualified Network.Stream as Net
import Text.Regex.Posix
import Data.Maybe
import TaskQueue
import HTTPWorker

-- Task i is to spider the (i % 1000000)th URL in the Result with id (i / 1000000)
million = 1000000 :: Int

requestBuilder :: String -> Task -> [Result [String]] -> HTTP.Request_String
requestBuilder defaultURL task results = HTTP.getRequest $ if dependencyIDs task == []
	then defaultURL
	else result !! index
		where
			index = taskID task `mod` million
			result = head $ filter (\r -> length r > index) $ map getResult results

extractHTTPLinks :: String -> [String]
extractHTTPLinks "" = []
extractHTTPLinks str = (\(x,y,z) -> if y == ""
						then []
						else (drop 9 $ init $ init y) : extractHTTPLinks z) match 
	where
		match = str =~ "<a href=\"http://[^\"]+\">" :: (String, String, String)

responseParser :: Int -> Task -> Net.Result (HTTP.Response String) -> IO (Response [String])
responseParser numWorkers task result = do
	code <- HTTP.getResponseCode result
	if code /= (2,0,0)
		then return $ Response Nothing []	-- Don't retry on errors
		else do
			body <- HTTP.getResponseBody result
			let newURLs = extractHTTPLinks body
			sequence $ map putStrLn newURLs
			let baseID = numWorkers + taskID task `div` million
			let newResult = Result baseID newURLs
			let newTasks = map (\n -> Task (million * baseID + n) [baseID]) [0 .. length newURLs - 1]
			return $ Response (Just newResult) newTasks

main :: IO ()
main = do
	args <- getArgs
	let numWorkers = read $ head args
	let timeout = read (args !! 1)
	let firstURL = args !! 2
	workers <- sequence $ map (\_ -> newHTTPWorker (requestBuilder firstURL) (responseParser numWorkers) timeout) [0 .. numWorkers - 1]
	let queue = newQueue workers [Task 0 []] :: Queue (HTTPWorker [String]) [String]
	run queue 100
