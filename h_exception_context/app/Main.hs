{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ConstraintKinds #-}

import Control.Exception

-- Define the context type
data ExceptionContext = ExceptionContext String deriving Show

-- Define a custom exception
data MyException = MyError String deriving Show

instance Exception MyException

-- Type alias for context requirement
type HasExceptionContext = (?exceptionContext :: ExceptionContext)

-- Function that throws an exception with context
throwWithContext :: MyException -> IO ()
throwWithContext ex = throwIO ex  -- Throwing MyException directly

main :: IO ()
main = do
    -- Setting up an implicit parameter for context
    let ?exceptionContext = ExceptionContext "Main Function"
    
    -- Catching the exception
    result <- try (throwWithContext (MyError "An error occurred!")) :: IO (Either MyException ())   
    case result of
        Left ex -> do   
            putStrLn $ "Caught an exception: " ++ show ex
            putStrLn $ "Context: " ++ show ?exceptionContext  -- Accessing the implicit parameter
        Right _ -> putStrLn "No exceptions occurred."
