module Day3 where

import Prelude hiding (Left, Right)
import Data.List.Split

filename = "input/day3"

data Direction = Up | Right | Down | Left deriving
    (Show, Eq)

instance Read Direction where
    readsPrec _ "U" = [(Up, "")]
    readsPrec _ "R" = [(Right, "")]
    readsPrec _ "D" = [(Down, "")]
    readsPrec _ "L" = [(Left, "")]
    readsPrec _ _ = []


type Coordinate = (Int, Int)
type Cable = [Coordinate]

data Movement = Movement Direction Int

instance Read Movement where
    readsPrec _ (d:l) = [(Movement (read [d]) (read l), "")]

parse :: String -> [Movement]
parse s = map read $ splitOn "," s

-- coordinates travelled when moving from a given start point
route :: Coordinate -> Movement -> [Coordinate]
route (x, y) (Movement Up d) = map (\y' -> (x, y+y')) [0..d]
route (x, y) (Movement Right d) = map (\x' -> (x+x', y)) [0..d]
route (x, y) (Movement Down d) = map (\y' -> (x, y-y')) [0..d]
route (x, y) (Movement Left d) = map (\x' -> (x-x', y)) [0..d]

cable :: [Movement] -> Cable 
cable moves = cable' moves [(0,0)] where
    cable' [] acc = acc
    cable' (m:ms) acc = cable' ms $ acc ++ route (last acc) m