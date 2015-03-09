-- Creates a worker which performs HTTP requests and processes the results
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module HTTPWorker where

import qualified Network.HTTP as HTTP
import qualified Network.Stream as Net
import Control.Concurrent
import Data.Either
import TaskQueue

data HTTPWorker a = HTTPWorker (Task -> [Result a] -> HTTP.Request_String) (Task -> Net.Result (HTTP.Response String) -> IO (Response a)) Int Bool (MVar Task) (MVar (Response a))

type HTTPQueue a = Queue (HTTPWorker a) a

newHTTPWorker :: (Task -> [Result a] -> HTTP.Request_String) -> (Task -> Net.Result (HTTP.Response String) -> IO (Response a)) -> Int -> Bool -> IO (HTTPWorker a)
newHTTPWorker requestBuilder responseParser timeout retry = do
	index <- newMVar (Task 0 [])	-- task is arbitrary, just needs to be non-empty so that swap doesn't block
	response <- newEmptyMVar
	return $ HTTPWorker requestBuilder responseParser timeout retry index response

buildRequest :: HTTPWorker a -> Task -> [Result a] -> HTTP.Request_String
buildRequest (HTTPWorker x _ _ _ _ _) = x

parseResponse :: HTTPWorker a -> Task -> Net.Result (HTTP.Response String) -> IO (Response a)
parseResponse (HTTPWorker _ y _ _ _ _) = y

getTimeout :: HTTPWorker a -> Int
getTimeout (HTTPWorker _ _ z _ _ _) = z

retryOnTimeout :: HTTPWorker a -> Bool
retryOnTimeout (HTTPWorker _ _ _ w _ _) = w

getTaskMVar :: HTTPWorker a -> MVar Task
getTaskMVar (HTTPWorker _ _ _ _ u _) = u

getResponseMVar :: HTTPWorker a -> MVar (Response a)
getResponseMVar (HTTPWorker _ _ _ _ _ v) = v

instance Queryable (HTTPWorker a) where
	ready worker = isEmptyMVar (getResponseMVar worker) >>= return . not

makeHTTPRequest :: (HTTPWorker a) -> Task -> [Result a] -> IO ()
makeHTTPRequest worker task results = do
	swapMVar (getTaskMVar worker) task
	httpResponse <- HTTP.simpleHTTP (buildRequest worker task results)
	parsedResponse <- parseResponse worker task httpResponse
	tryPutMVar (getResponseMVar worker) parsedResponse
	return ()

onTimeout :: HTTPWorker a -> ThreadId -> IO ()
onTimeout worker threadID = do
	task <- readMVar $ getTaskMVar worker
	timedOut <- tryPutMVar (getResponseMVar worker) $ Response Nothing $ if retryOnTimeout worker
																			then [task]
																			else []
	if timedOut
		then killThread threadID >> putStrLn "timeout"
		else return ()

instance Worker (HTTPWorker a) a where
	send worker task results = do
		threadID <- forkIO $ makeHTTPRequest worker task results
		forkIO (threadDelay (getTimeout worker) >> onTimeout worker threadID)
		return ()

	receive worker = takeMVar $ getResponseMVar worker
