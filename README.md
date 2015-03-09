Haskell-Task-Queue
------------------

A queue written in Haskell to facilitate concurrent computations.
It follows an actor model, allowing for dependencies between tasks, so long as they are cycle-free.
The queue does not spawn threads for workers, leaving these details to the workers.
It does not support worker capabilities, i.e. every worker must be able to process every task.

Queues consist of workers, tasks and results.
Results each have an ID associated with them.
Tasks contain simply an ID and the IDs of results they depend on.
There is no need for IDs to be unique.

Queues can be either stateless or stateful.
A stateless queue does no processing of the results,
and must be periodically blocked for garbage collection of unneeded results.
A stateful queue processes results between each round of polling workers and round of assigning tasks,
and is assumed to handle garbage collection itself.

Workers must satisfy the following properties:
 - Stateless
 - A "send" function which initiates processing a task using the results of tasks it depends on
 - A "receive" function which returns the either the result of the task or failure, and a list
of new tasks to add to the queue
 - A "ready" function which reports whether "receive" is ready to be called

The queue gives the following guarantees:
 - Each task will be processed at most once
 - If all a task's dependencies' results exist in the queue, it will eventually be processed (no deadlock)
 - If all a task's dependencies' results exist in the queue, it will be processed before any task added to the queue after it (FIFO)

The garbage collection for the stateless queue also guarantees that
results will not be deleted unless no task is in progress and no task in the queue depends on them.

API:
```haskell
data Task = Task Int [Int]
data Result a = Result Int a
data Response a = Response (Maybe (Result a)) [Task]

class Queryable w where
	ready :: w -> IO Bool
class (Queryable w) => Worker w a where
	send :: w -> Task -> [Result a] -> IO ()
	receive :: w -> IO (Response a)

newQueue :: [w] -> [Task] -> Queue w a
run :: (Worker w a) => Queue w a -> Int -> IO ()

type StatefulQueue s w a = StateT s IO (Queue w a)

runWithState :: (Worker w a) => StatefulQueue s w a -> (StatefulQueue s w a -> StatefulQueue s w a) -> StateT s IO ()
```

### HTTPWorker

This module makes it easier to define workers which make an HTTP request and do something with the result.
It handles timeouts and error conditions sanely: timeouts resulting in re-queueing while HTTP errors do not.

### TODO:

 - Implement various standard types of workers
 - Health checks
 - Smarter garbage collection?
