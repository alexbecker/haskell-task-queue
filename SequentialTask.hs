-- Test tasks for TaskQueue
-- Must be executed in order, so no concurrency exists
-- Prints natural numbers sequentially
{-# LANGUAGE MultiParamTypeClasses #-}
module Main where

import Control.Concurrent
import Data.Maybe
import TaskQueue

data IntWorker = IntWorker (MVar Int)

newIntWorker :: IO IntWorker
newIntWorker = newEmptyMVar >>= return . IntWorker

instance Queryable IntWorker where
	ready (IntWorker response) = isEmptyMVar response >>= (return . not)

instance Worker IntWorker Int where
	send (IntWorker response) task results = forkIO ((putStrLn $ show $ val) >> putMVar response val) >> return () where
		val = if dependencyIDs task == []
				then 0
				else 1 + (fromJust $ lookupResult (head $ dependencyIDs task) results)
	receive (IntWorker response) = do
		n <- takeMVar response
		return $ Response (Just $ Result n n) [Task (n + 1) [n]]

main :: IO ()
main = do
	workers <- sequence $ replicate 5 newIntWorker
	let queue = Queue workers [] [Task 0 []] [] :: Queue IntWorker Int
	run queue 10
