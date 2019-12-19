module Day2 where

import Control.Lens
import Data.List.Split

filename = "input/day2"

type Program = [Int]

-- replace position 1 with the value 12
-- replace position 2 with the value 2
initialise :: Program -> Program
initialise program = program & ix 1 .~ 12 & ix 2 .~ 2

-- apply a function to the values at indexes i+1 and i+2
-- write the result to the index at the value of i+3
operate :: (Int -> Int -> Int) -> Program -> Int -> Maybe Program
operate f program i = do
    a <- get (i + 1) >>= get
    b <- get (i + 2) >>= get
    dest <- get (i + 3)
    Just $ program & ix dest .~ f a b
    where get i = program ^? ix i

run :: Program -> Maybe Program 
run program = run' 0 (initialise program) where 
    run' i program = case program ^? ix i of
        Nothing -> Just program
        Just 99 -> Just program
        Just 1 -> operate (+) program i >>= run' (i + 4)
        Just 2 -> operate (*) program i >>= run' (i + 4)

solve :: IO (Maybe Int)
solve = do 
    input <- readFile filename 
    let program = map read $ splitOn "," input
    return $ run program >>= (\p -> p ^? ix 0)