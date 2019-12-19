module Day2 where

import Control.Lens
import Data.List
import Data.List.Split

filename = "input/day2"
target = 19690720

type Program = [Int]

initialise :: Program -> Int -> Int -> Program
initialise program noun verb = program & ix 1 .~ noun & ix 2 .~ verb

-- apply a function to the values at indexes i+1 and i+2
-- write the result to the index at the value of i+3
operate :: (Int -> Int -> Int) -> Program -> Int -> Maybe Program
operate f program i = do
    a <- get (i + 1) >>= get
    b <- get (i + 2) >>= get
    dest <- get (i + 3)
    Just $ program & ix dest .~ f a b
    where get i = program ^? ix i

run :: Program -> Int -> Int -> Maybe Program 
run program noun verb = run' 0 (initialise program noun verb) where 
    run' i program = case program ^? ix i of
        Nothing -> Just program
        Just 99 -> Just program
        Just 1 -> operate (+) program i >>= run' (i + 4)
        Just 2 -> operate (*) program i >>= run' (i + 4)

solve :: Int -> IO (Maybe Int)
solve target = do 
    input <- readFile filename 
    let program = map read $ splitOn "," input
    let inputs = [(n, v) | n <- [0..99], v <- [0..99]]
    return $ find (\(n, v) -> (run program n v >>= (\p -> p ^? ix 0)) == Just target) inputs >>= 
        (\r -> Just $ 100 * fst r + snd r)
        
solve' = solve target