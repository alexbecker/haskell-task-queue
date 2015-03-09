Haskell-Task-Queue
------------------

A queue written in Haskell to facilitate concurrent computations.
It follows an actor model, and allows for dependencies between tasks,
so long as they are cycle-free. The queue does not spawn threads for workers and does
no processing of the results, leaving all these details to the workers.
It does not support worker capabilities, i.e. every worker must be able to process every task.
The queue must also be periodically blocked for garbage collection of unneeded results.

Queues consist of workers, tasks and results.
Tasks contain simply an ID and the IDs of tasks they depend on.
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
 - Results will not be deleted unless no task is in progress and no task in the queue depends on them

### TODO:

 - Implement various standard types of workers
 - Health checks
 - Smarter garbage collection?
