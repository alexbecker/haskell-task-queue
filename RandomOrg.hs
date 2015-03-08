-- Test tasks for TaskQueue
-- Makes concurrent queries to RANDOM.ORG
-- ./RandomOrg k prints permutations from RANDOM.ORG of [1..n] using k threads
-- Each value of n is obtained from RANDOM.ORG as well, and has 5 uses
{-# LANGUAGE MultiParamTypeClasses #-}
module Main where

import Control.Concurrent
import System.Environment
import Network.HTTP.Conduit
import Network.Connection
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as C
import Data.Maybe
import Data.List
import Debug.Trace
import TaskQueue

randOrgTasks = concat $ map (\n -> Task n [] : map (Task 0) (replicate 5 [n])) [1..100]

netManager = newManager $ mkManagerSettings (TLSSettingsSimple True False False) Nothing

randIntQuery :: IO Request
randIntQuery = parseUrl "https://www.random.org/integers/?num=1&min=2&max=10&col=1&base=10&format=plain&rnd=new"

randInt :: IO Int
randInt = do
	manager <- netManager
	query <- randIntQuery
	response <- httpLbs query manager
	return $ read $ C.unpack $ L.toStrict $ responseBody response

randSeqQuery :: Int -> IO Request
randSeqQuery n = parseUrl $ "https://www.random.org/sequences/?min=1&max=" ++ show n ++ "&col=11&format=plain&rnd=new"

printRandSeq :: Int -> IO ()
printRandSeq n = do
	manager <- netManager
	query <- randSeqQuery n
	response <- httpLbs query manager
	C.putStrLn $ L.toStrict $ responseBody response

data RandOrgWorker = RandOrgWorker (MVar Int) (MVar Int)

newROWorker :: IO RandOrgWorker
newROWorker = do
	rand <- newEmptyMVar
	index <- newEmptyMVar
	return $ RandOrgWorker rand index

instance Queryable RandOrgWorker where
	ready (RandOrgWorker rand _) = isEmptyMVar rand >>= return . not

instance Worker RandOrgWorker Int where
	send (RandOrgWorker rand index) task results = if taskID task == 0
		then forkIO (do
			printRandSeq $ fromJust $ lookupResult (head $ dependencyIDs task) results
			putMVar rand 0) >> return ()
		else forkIO (do
			putMVar index $ taskID task
			randInt >>= putMVar rand) >> return ()
	receive (RandOrgWorker rand index) = do
		i <- takeMVar rand
		if i == 0
			then return $ TaskQueue.Response (Just (Result 0 i)) []
			else do
				ind <- takeMVar index
				return $ TaskQueue.Response (Just (Result ind i)) []

main :: IO ()
main = do
	args <- getArgs
	let numWorkers = read $ head args
	workers <- sequence $ replicate numWorkers newROWorker
	let queue = Queue workers [] randOrgTasks [] :: Queue RandOrgWorker Int
	run queue 100
