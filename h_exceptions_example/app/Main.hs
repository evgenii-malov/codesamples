import Control.Monad.Except

type Error = String
type Calculation = Except Error Int

safeDivide :: Int -> Int -> Calculation
safeDivide _ 0 = throwError "Division by zero"
safeDivide x y = return (x `div` y)

performCalculation :: Calculation
performCalculation = do
    a <- safeDivide 10 2
    b <- safeDivide a 0  -- This will throw an error
    return b

main :: IO ()
main = do
    let result = runExcept performCalculation
    case result of
        Left err -> putStrLn $ "Error: " ++ err
        Right val -> print val
