{-# LANGUAGE MultiParamTypeClasses #-}
module TaskQueue where

import Data.List
import Data.Maybe
import Data.Tuple
import Data.Function

data Task = Task Int [Int]
	deriving (Show)

instance Eq Task where
	(==) = (==) `on` taskID

taskID :: Task -> Int
taskID (Task x _ ) = x

dependencyIDs :: Task -> [Int]
dependencyIDs (Task _ y) = y

data Result a = Result Int a
	deriving (Show)

getResultID :: Result a -> Int
getResultID (Result x _) = x

getResult :: Result a -> a
getResult (Result _ y) = y

lookupResult :: Int -> [Result a] -> Maybe a
lookupResult n results = find ((n ==) . getResultID) results >>= return . getResult

-- extracts results for dependencies of a task
relevant :: Task -> [Result a] -> [Result a]
relevant t = filter ((flip elem (dependencyIDs t)) . getResultID)

data Response a = Response (Maybe (Result a)) [Task]
	deriving (Show)

success :: Response a -> Bool
success (Response x _) = isJust x

result :: Response a -> Result a
result (Response x _) = fromJust x

getTasks :: Response a -> [Task]
getTasks (Response _ y) = y

class Queryable w where
	ready :: w -> IO Bool

class (Queryable w) => Worker w a where
	send :: w -> Task -> [Result a] -> IO ()
	receive :: w -> IO (Response a)

-- values represent readyWorkers, busyWorkers, tasks, results
data Queue w a = Queue [w] [w] [Task] [Result a]

newQueue :: [w] -> [Task] -> Queue w a
newQueue workers tasks = Queue workers [] tasks []

getResults :: Queue w a -> [Result a]
getResults (Queue _ _ _ results) = results

readyWorkers :: Queue w a -> [w]
readyWorkers (Queue workers _ _ _) = workers

busyWorkers :: Queue w a -> [w]
busyWorkers (Queue _ workers _ _) = workers

allTasks :: Queue w a -> [Task]
allTasks (Queue _ _ tasks _) = tasks

readyTasks :: Queue w a -> [Task]
readyTasks (Queue _ _ tasks results) = filter (and . map (flip elem (map getResultID results)) . dependencyIDs) tasks

setStatus :: [w] -> Bool -> [(w, Bool)]
setStatus workers = zip workers . replicate (length workers)

assignTasks :: (Worker w a) => Queue w a -> IO (Queue w a)
assignTasks queue = do
	sequence_ $ map (\(w,t) -> send w t (relevant t results)) $ zip usedWorkers assignedTasks
	return $ Queue unusedWorkers (usedWorkers ++ busyWorkers queue) (allTasks queue \\ assignedTasks) results
	where
		tasks = readyTasks queue
		workers = readyWorkers queue
		usedWorkers = take (length tasks) workers
		unusedWorkers = drop (length tasks) workers
		assignedTasks = take (length workers) tasks
		results = getResults queue

pollWorkers :: (Worker w a) => Queue w a -> IO (Queue w a)
pollWorkers queue = do
	let wereBusy = busyWorkers queue
	statuses <- sequence $ map ready wereBusy
	let finishedIndices = filter (statuses !!) [0 .. length statuses - 1]
	let notBusy = map (wereBusy !!) finishedIndices
	let stillBusy = map (wereBusy !!) $ [0 .. length statuses - 1] \\ finishedIndices
	responses <- sequence $ map receive notBusy
	let newResults = map result $ filter success responses
	let newTasks = concat $ map getTasks responses
	return $ Queue (notBusy ++ readyWorkers queue) stillBusy (allTasks queue ++ newTasks) (getResults queue ++ newResults)

block :: (Worker w a) => Queue w a -> IO (Queue w a)
block queue = do
	statuses <- sequence $ map ready $ busyWorkers queue
	if and statuses
		then return queue
		else block queue

gc :: (Worker w a) => Queue w a -> Queue w a
gc (Queue w1 w2 tasks results) = Queue w1 w2 tasks $ filter (\r -> getResultID r `elem` needed) results where
	needed = nub $ concat $ map dependencyIDs tasks

run :: (Worker w a) => Queue w a -> Int -> IO ()
run queue gcInterval = do
	queue <- foldl1 (.) (replicate gcInterval ((>>= pollWorkers) . (>>= assignTasks))) $ return queue
	queue <- block queue
	queue <- pollWorkers queue
	run (gc queue) gcInterval
