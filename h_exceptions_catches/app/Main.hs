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
f 1 = error "oops error in clean"
f 2 = throw $ MyException "Error data"

-- so result of clean func can't be catched even if we wrap it in IO via return
wf = return (f 1) :: IO Int
wef = evaluate (f 1)

g :: Int -> IO Int
g 0 = return 1
g 1 = error "oops error in dirty"
g 2 = throwIO $ MyException "Error data throwIO"


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

    gv <- catch (g 1) (\(ErrorCall _) -> print 1 >> return 200)
    putStrLn $ "Result of dirty func: " ++ show gv
    
    -- but why with evaluate we can catch ? 
    x <- catch (wef) (\(ErrorCall _) -> print "Catched clean func error call" >> return 200)
    putStrLn $ "Result of clean func wrapped with evaluete: " ++ show x  

    -- so result of clean func can't be catched even if we wrap it in IO via return
    x <- catch (wf::IO Int) (\(ErrorCall _) -> print 1 >> return 200)
    putStrLn $ "Result of clean func wrapped with return: " ++ show x  -- we did't get here
{-
You cannot catch exceptions created by the error function in pure functions because they are not designed to handle such situations. Instead, you should use safer constructs that allow for proper error handling within the context of Haskell's type system and functional programming principles. If you need to handle errors gracefully, consider using types like Maybe, Either, or defining your own exception types within the IO monad.

Why It Doesn't Work: The expression return (error "ops") does not force evaluation of (error "ops"). Instead, it wraps the unevaluated expression in an IO action. Since nothing is evaluated until you actually use the result (which doesn’t happen here), the exception is not thrown at that point, and thus it cannot be caught by the handler.
Summary
Catching Exceptions: To catch exceptions from functions like error, you must ensure that the expression is evaluated. Using evaluate achieves this by forcing evaluation.
Using Return: Simply wrapping an expression in return does not evaluate it; hence, any exceptions thrown remain unevaluated and uncaught until they are forced later in execution.
Conclusion
In summary, if you want to catch exceptions from expressions that may fail (like those using error), you must force their evaluation using constructs like evaluate. Using just return will not work because it does not evaluate the expression, leaving any potential exceptions uncaught.
-}


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
