import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception (bracket, AsyncException(UserInterrupt), catch)
import Control.Monad (unless)

-- TODO MULTIPLE THREADS

main :: IO ()
main = do
    putStrLn "Press Ctrl+C to terminate the program."

    -- Use bracket to manage the worker thread's lifecycle
    bracket startWorker cleanup $ \worker -> do
        -- Wait for the worker thread to finish
        wait worker

-- Function to start the worker thread
startWorker :: IO (Async ())
startWorker = async $ workerThread

-- Worker thread function
workerThread :: IO ()
workerThread = do
    let loop = do
            putStrLn "Worker thread is running..."
            threadDelay 1000000  -- Delay for 1 second
            loop
    loop `catch` handleInterrupt

-- Handle user interrupt gracefully
handleInterrupt :: AsyncException -> IO ()
handleInterrupt UserInterrupt = putStrLn "Worker thread was interrupted by the user."

-- Cleanup function to be called when exiting the program
cleanup :: Async () -> IO ()
cleanup worker = do
    cancel worker  -- Cancel the worker thread if it's still running
    putStrLn "Cleaning up resources..."

