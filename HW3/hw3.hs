main :: IO ()
main = print "2002688"

insonnia :: String
insonnia = concatMap (\x -> show x ++ " sheep ") [1..]

-- anche iterate (\xs -> 1: zipWith (+) xs (tail xs) ++ [1]) [1]
tartaglia :: [[Integer]]
tartaglia = [1] : tartaglia'
  where tartaglia' = map (\xs -> 1: zipWith (+) xs (tail xs) ++ [1]) tartaglia

luckyNumbers :: [Int]
luckyNumbers = luckyNumbers' (filter (\x -> x `mod` 2 == 1) [1..]) 1
  where
    luckyNumbers' (1:xs) indx = 1 : luckyNumbers' xs (indx +1)
    luckyNumbers' (x:xs) indx = x : luckyNumbers' newLuckynumbers (indx +1)
      where
        newLuckynumbers = map fst (filter (\(_, y) -> (y `mod` x) /= 0) (zip xs [(indx+1)..]))

primRec :: (Integer -> a -> a) -> a -> Integer -> a      
primRec h g 0 = g
primRec h g n = h (n -1) (primRec h g (n-1))

primRec' :: (Integer -> a -> a) -> a -> Integer -> a
primRec' h g n = snd (for n (\(i, acc) -> (i+1, h i acc)) (0, g))
    where
        for 0 f x = x
        for n f x = for (n-1) f (f x)

zero = \f x -> x
succ n = \f x -> f (n f x)

primRecChurch h g n = n h g

ackermann :: Integer -> Integer -> Integer
ackermann m n = 1 + primRec h n m
  where
    h _ 0 = 1
    h m' n' = ackermann (m - m') (n -1)

partsFromAll :: Int -> [[Int]] -> [[Int]]
partsFromAll n xss = reverse([take i xs |i <- [1..n], xs <- takeWhile (\xs -> head xs < n) xss, sum (take i xs) == n]) ++ [[n]]

repeatOne :: [Int]
repeatOne = 1: repeatOne

checkOrd :: Ord a => [a] -> Bool
checkOrd [x] = True
checkOrd (x:y:[]) = x < y

addOneToLast :: [Int] -> [Int]
addOneToLast [] = []
addOneToLast [x] = (x + 1):repeatOne 
addOneToLast (x:xs) = x : addOneToLast xs

allPartitions :: [[Int]]
allPartitions = [repeatOne] ++ nextParts 2
  where
    nextParts 1 = [repeatOne]
    nextParts i = newParts (partsFromAll (i-1) allPartitions) ++ nextParts (i+1)
      where
        newParts xss = [addOneToLast xs | xs <- xss, checkOrd (take 2 (reverse xs))]

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
  | x > y = y : merge (x:xs) ys
  | otherwise = x : merge xs (y:ys)

getFirstNoRep :: Eq(a) => [a] -> (a, [a])
getFirstNoRep [x] = (x, [])
getFirstNoRep (x:y:xs)
  | x == y = getFirstNoRep (dropWhile (==x) xs)
  | otherwise = (x, y:xs)

ulams :: [Int]
ulams = 1:2:(findCandidates (tail(ulams)) [])
  where
    findCandidates (x:xs) ys = fstNoRep : findCandidates xs nexts
      where
        (fstNoRep, nexts) = getFirstNoRep (ys `merge` map (+x) (takeWhile (< x) ulams))


data BinTree a = Empty | Node a (BinTree a) (BinTree a) deriving (Show)

calvinWilfTree :: BinTree (Int, Int)
calvinWilfTree = makeNode (1, 1)
  where 
    makeNode (m, n) = Node (m, n) (makeNode (m, n + m)) (makeNode (m + n, n))

takeNlevels :: Int -> BinTree a -> BinTree a
takeNlevels 0 _ = Empty
takeNlevels lvl (Node x l r) = Node x (takeNlevels (lvl-1) l) (takeNlevels (lvl-1) r)

visitaLivelli :: Ord(a) => BinTree a -> [a]
visitaLivelli (Node x l r) = map snd (zipTreeToLvl 0 (Node x l r))
  where
    zipTreeToLvl _ Empty = []
    zipTreeToLvl n (Node x l r) = (n, x) : merge (zipTreeToLvl (n +1) l) (zipTreeToLvl (n +1) r)

