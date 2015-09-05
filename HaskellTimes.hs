import System.Random
import Data.List.Split

checkingThings :: Int -> Int -> [[Int]] -> Int
checkingThings i j world =
    
    let isAlive = world!!i!!j == 1
    in if isAlive 
        then (if sumNeighbours i j world == 2 || sumNeighbours i j world == 3 
            then 1 
            else 0) 
        else (if sumNeighbours i j world == 3 
            then 1 
            else 0)


sumNeighbours:: Int -> Int -> [[Int]] -> Int
sumNeighbours i j world = sum [get (i-1) (j-1) world, get (i-1) j world, get (i-1) (j+1) world, get i (j-1) world, get i (j+1) world, get (i+1) (j-1) world, get (i+1) j world, get (i+1) (j+1) world]

playTheGame :: [[Int]] -> [[Int]]
playTheGame (x:xs) = x:xs
    -- in nextGen = (checkingThings row col xs):playTheGame xs 

get :: Int -> Int -> [[Int]] -> Int
get i j world = 
    let i' = if i<0 
        then length world -1
        else if i> length world -1
                then 0
                else i
        j' = if j<0
        then length (head world) -1 
        else if j> length (head world) -1
                then 0
                else j
    in world!!i'!!j'

main = do
  g <- newStdGen
  let universe = splitPlaces (replicate 10 10) (take 100 $ (randomRs (0, 1) g :: [Int]))
  print universe
  print $ playTheGame universe