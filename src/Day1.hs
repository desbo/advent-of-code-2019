module Day1
    ( fuelRequired
    , solve
    ) where

filename = "input/day1"

fuelRequired :: Int -> Int
fuelRequired mass = go mass 0 where
    go mass acc
        | mass <= 0 = acc
        | otherwise = go fuel (acc + max 0 fuel) 
        where fuel = (mass `div` 3) - 2

solve :: IO Int
solve = do 
    input <- readFile filename 
    let masses = map (\x -> fuelRequired $ read x) $ lines input
    return $ sum masses