-- https://downloads.haskell.org/~ghc/5.00/docs/set/sec-ghc-concurrency.htm
--
-- If you want the program to wait for child threads to finish before exiting, you need to program this yourself. A simple mechanism is to have each child thread write to an MVar when it completes, and have the main thread wait on all the MVars before exiting
--
-- GHC implements pre-emptive multitasking: the execution of threads are interleaved in a random fashion. More specifically, a thread may be pre-empted whenever it allocates some memory, which unfortunately means that tight loops which do no allocation tend to lock out other threads (this only seems to happen with pathalogical benchmark-style code, however).
-- 
--
-- The rescheduling timer runs on a 20ms granularity by default, but this may be altered using the -i<n> RTS option. After a rescheduling “tick” the running thread is pre-empted as soon as possible.
--
import Control.Exception
import Control.Concurrent
import Control.Concurrent.MVar (newMVar, readMVar, takeMVar, putMVar)
import Control.Monad

main :: IO ()
main = do
    putStrLn "Нажмите Ctrl+C для завершения."

    -- Создаем MVar для управления состоянием завершения
    stopSignal <- newMVar False

    -- Запускаем второй поток с использованием bracket
    let workerThread = bracket acquireResource releaseResource $ \resource -> do
            -- Рабочий поток, который будет выполнять задачу
            let loop = do
                    stop <- readMVar stopSignal
                    unless stop $ do
                        putStrLn "Рабочий поток работает..."
                        threadDelay 1000000  -- Задержка 1 секунда
                        loop
            loop

    -- Запускаем рабочий поток
    _ <- forkIO workerThread

    -- Основной цикл программы с обработкой исключений
    handle (handler stopSignal) $ do
        let loop = do
                putStrLn "Основной поток работает..."
                threadDelay 1000000  -- Задержка 1 секунда
                loop
        loop

-- Функция для выделения ресурса
acquireResource :: IO String
acquireResource = do
    putStrLn "Ресурс выделен."
    return "Ресурс"

-- Функция для освобождения ресурса
releaseResource :: String -> IO ()
releaseResource resource = putStrLn $ "Ресурс освобожден: " ++ resource

-- TODO why this not work ?

-- Обработчик для обработки асинхронных исключений
handler :: MVar Bool -> AsyncException -> IO ()
handler stopSignal UserInterrupt = do
    putStrLn "Программа была прервана пользователем."
    -- Устанавливаем сигнал остановки для рабочего потока
    takeMVar stopSignal `catch` \(SomeException _)-> do
        --putMVar stopSignal True
	putStrLn "catched some!"
    	return False  -- Игнорируем возможное исключение при захвате MVar
    putMVar stopSignal True  -- Устанавливаем флаг завершения

handler _ ex = putStrLn $ "Произошло другое исключение: " ++ show ex
