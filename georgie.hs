maximum' :: (Ord a) => [a] -> a
maximum'[x] = x
maximum'(x:xs)
    | x > maxTail = x
    |otherwise = maxTail
    where maxTail = maximum' xs

replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
    | n <= 0 = []
    | otherwise = x:replicate' (n-1) x

take' :: (Num i , Ord i) => i -> [a] -> [a]
take' n _
    | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x : take' (n-1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y):zip' xs ys


elem' :: (Eq a) => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs)
    | a == x = True
    | otherwise = elem' a xs

qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort (x:xs) = 
    let leftSort = qsort [a | a <- xs, a <= x]
        rightSort = qsort [a | a <- xs, a > x]
    in leftSort ++ [x] ++ rightSort

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
    | p x = x : filter' p xs
    | otherwise = filter' p xs

qsort' :: (Ord a) => [a] -> [a]
qsort' [] = []
qsort' (x:xs) =
    let leftSort = qsort' (filter' (<= x) xs)
        rightSort = qsort' (filter' (> x) xs)
    in leftSort ++ [x] ++ rightSort

allDivisible :: (Integral a) => a -> [a] -> [a]
allDivisible _ [] = []
allDivisible 0 _ = []
allDivisible y (x:xs) = filter p xs
    where p x = x `mod` y == 0

chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
    | even n = n:chain (n `div` 2)
    | odd n = n:chain (n * 3 + 1)

collatz :: (Integral a) => a -> a
collatz 0 = error "nope"
collatz n = fromIntegral (length (chain n))

numLongChains :: Int
numLongChains = length (filter isLong (map chain[1..100]))
    where isLong xs = length xs > 15

addThree :: (Num a) => a -> a -> a -> a
addThree = \x -> \y -> \z -> x + y + z

flip' :: (a -> b -> c) -> b -> a -> c
flip' f = \x y -> f y x

elemy' :: (Eq a) => a -> [a] -> Bool
elemy' y ys = foldl (\acc x -> if x==y then True else acc) False ys

mapr :: (a -> b) -> [a] -> [b]
mapr f xs = foldr (\x acc -> f x : acc) [] xs

mapl :: (a -> b) -> [a] -> [b]
mapl f xs = foldl (\acc x -> acc ++ [f x]) [] xs

sqrtSums :: Int
sqrtSums = length (takeWhile (<100)(scanl1 (+) (map sqrt [1..]))) + 1

sum' :: (Num a) => [a] -> a
sum' xs = foldl (+) 0 xs

sum'' :: (Num a) => [a] -> a
sum'' = foldl (+) 0

fun x = ceiling (negate (tan (cos (max 50 x))))
fun' = ceiling . negate . tan . cos . max 50

oddSumSquare :: Integer
oddSumSquare = sum (takeWhile (<1000) (filter odd (map (^2) [1..])))

oddSumSquare' :: Integer
oddSumSquare' = sum . takeWhile (<1000) . filter odd . map (^2) $ [1..]

oddSumSquare'' :: Integer
oddSumSquare'' = 
    let oddSquares = filter odd $ map (^2) [1..]
        belowLimit = takeWhile (<1000) oddSquares
    in sum belowLimit



