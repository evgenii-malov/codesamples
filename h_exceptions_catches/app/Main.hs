--https://github.com/devalot/hs-exceptions
import Control.Exception
import System.IO (hPutStrLn, stderr)

-- Define custom exception types
data MyException = MyException String deriving Show
data AnotherException = AnotherException String deriving Show

instance Exception MyException
instance Exception AnotherException

-- A function that can throw different exceptions based on input
dangerousFunction :: Int -> IO Int
dangerousFunction x = do
    putStrLn $ "Performing a dangerous operation with input: " ++ show x
    case x of
        1 -> throwIO (MyException "A custom exception occurred for input 1!")
        2 -> throwIO (AnotherException "Another exception occurred for input 2!")
	3 -> error "Ooops"
	4 -> throwIO (userError "This is a user-defined error!")
        _ -> return (x * 2)  -- Successful result for other inputs

f 0 = 1
f 1 = error "oops"
f 2 = throw $ MyException "Error data"

z = return (f 1) :: IO Int

-- Main function to demonstrate catching multiple exceptions
main :: IO ()
main = do
    result <- catches 
        (dangerousFunction 3)  -- Change this value to test different cases
        [ Handler handleMyException
        , Handler handleAnotherException
        , Handler handleErrorCall
        --, Handler handleIOError
        , Handler handleSomeCall  -- this is BAD use bracket or finally!
        ]
    
    putStrLn $ "Result: " ++ show result
    x <- catch (z::IO Int) (\(ErrorCall _) -> print 1 >> return 200)
    putStrLn $ "Result: " ++ show x

-- Handler for MyException
handleMyException :: MyException -> IO Int
handleMyException (MyException msg) = do
    hPutStrLn stderr $ "Caught MyException: " ++ msg
    return (-1)  -- Return a default value

-- Handler for AnotherException
handleAnotherException :: AnotherException -> IO Int
handleAnotherException (AnotherException msg) = do
    hPutStrLn stderr $ "Caught AnotherException: " ++ msg
    return (-2)  -- Return a different default value

-- Handler for user errors (default handler)
handleErrorCall :: ErrorCall -> IO Int
handleErrorCall e = do
    hPutStrLn stderr $ "Caught ErrorCall from error func: " ++ show e
    return (-3)  -- Return a different default value

    
handleSomeCall :: SomeException -> IO Int
handleSomeCall e = do
    hPutStrLn stderr $ "Caught SomeException: " ++ show e
    return (-4)  -- Return a different default value

handleIOError :: IOError -> IO Int
handleIOError e = do
    hPutStrLn stderr $ "Caught IOError: " ++ show e
    return (-4)  -- Return a different default value

{-
3. *Specificity*:
   - Unlike SomeException, which can represent any exception, ErrorCall specifically indicates that a runtime error has occurred due to improper use of functions like error.
   - This makes it easier to identify programming errors during development.
-}
