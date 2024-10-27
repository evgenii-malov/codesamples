-- TO DO SIMULT WORK
import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception (bracket)
import Control.Monad (replicateM, forM_)
import System.Random (randomRIO)

type Task = IO ()

-- Function to create a thread pool
threadPool :: Int -> [Task] -> IO ()
threadPool numWorkers tasks = do
    -- Create a MVar to hold the tasks
    taskQueue <- newMVar tasks

    -- Create worker threads
    workers <- replicateM numWorkers $ async $ worker taskQueue

    -- Wait for all workers to finish
    waitAll workers

-- Worker function that processes tasks from the queue
worker :: MVar [Task] -> IO ()
worker taskQueue = do
    -- Process tasks until there are none left
    let loop = do
            -- Take the current list of tasks
            tasks <- takeMVar taskQueue
            
            case tasks of
                [] -> return ()  -- No more tasks to process
                (task:rest) -> do
                    task  -- Execute the task
                    putMVar taskQueue rest  -- Put remaining tasks back in the queue
                    loop  -- Continue processing

    loop

-- Wait for all worker threads to finish
waitAll :: [Async ()] -> IO ()
waitAll workers = forM_ workers wait

-- Function to create a random delay task
randomDelayTask :: Int -> Task
randomDelayTask taskId = do
    delay <- randomRIO (1, 5)  -- Random delay between 1 and 5 seconds
    putStrLn $ "Task " ++ show taskId ++ " will run for " ++ show delay ++ " seconds."
    threadDelay (delay * 1000000)  -- Convert seconds to microseconds

main :: IO ()
main = do
    let numWorkers = 2  -- Set number of worker threads to 2 for simultaneous execution
        tasks = [randomDelayTask i | i <- [1..10]]  -- Sample tasks with random delays

    putStrLn "Starting thread pool..."
    
    -- Run the thread pool with resource management using bracket
    bracket (return ()) (const $ putStrLn "Cleaning up resources...") $ \_ ->
        threadPool numWorkers tasks
    
    putStrLn "All tasks have been completed."
