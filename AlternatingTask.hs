-- Test tasks for TaskQueue
-- Two workers must always alternate
-- should print hello and world on consecutive lines repeatedly
{-# LANGUAGE MultiParamTypeClasses #-}
module Main where

import Control.Concurrent
import TaskQueue

helloWorldTasks = Task 0 [] : map (\n -> Task (mod n 2) [mod (n + 1) 2]) [0..99]

data StringWorker = StringWorker [String] (MVar Int)

helloWorldWorker :: IO StringWorker
helloWorldWorker = newEmptyMVar >>= return . (StringWorker ["hello","world"])

instance Queryable StringWorker where
	ready (StringWorker _ mvar) = isEmptyMVar mvar >>= return . not

instance Worker StringWorker () where
	send (StringWorker strings mvar) task results = forkIO (putStrLn (strings !! (taskID task)) >> putMVar mvar (taskID task)) >> return ()
	receive (StringWorker _ mvar) = do 
		oldID <- takeMVar mvar
		return $ Response (Just $ Result oldID ()) []

main :: IO ()
main = do
	workers <- sequence $ replicate 2 helloWorldWorker
	let queue = Queue workers [] helloWorldTasks [] :: Queue StringWorker ()
	run queue 10
