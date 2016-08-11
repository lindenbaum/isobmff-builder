module Main where

-- TODO fill in real stuff

import Data.Time.Clock

fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

main = do
    start <- getCurrentTime
    let !r = fib 30
    end <- getCurrentTime
    putStrLn $ "fib 30 took " ++ show (diffUTCTime end start)
