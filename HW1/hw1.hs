main :: IO ()
main = print ([0..5])

-- assumiamo che il costo sia n log n
sortBy :: (Ord b) => (a -> b) -> [a] -> [a]
sortBy p [] = []
sortBy p (x:xs) =
  let smallerSorted = sortBy p [a | a <- xs, p a <= p x]
      biggerSorted = sortBy p [a | a <- xs, p a > p x]
  in  smallerSorted ++ [x] ++ biggerSorted

-- Eercizio 1.1
myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile p [] = []
myTakeWhile p (x:xs) = 
  if p x then x : myTakeWhile p xs
  else []

myDropWhile :: (a -> Bool) -> [a] -> [a]
myDropWhile p [] = []
myDropWhile p (x:xs) = 
  if p x then myDropWhile p xs
  else x:xs

-- Esercizio 1.2
-- un'altra soluzione sarebbe con null myTakeWhile
myRemoveDupsOrd :: Ord a => [a] -> [a]
myRemoveDupsOrd [] = []
myRemoveDupsOrd xs = head xs : map (snd) (filter (uncurry (/=)) (zip xs (tail xs)))
-- myRemoveDupsOrd (x:xs)
-- | null xs = [x]
-- | x == head xs = myRemoveDupsOrd xs
-- | otherwise = x : myRemoveDupsOrd xs

-- Esercizio 1.3
myRemoveDupsOrdBy :: Ord b => (a -> b) -> [a] -> [a]
myRemoveDupsOrdBy _ [] = []
myRemoveDupsOrdBy p xs = (head xs) : map snd (filter (\(x, y) -> p x /= p y) (zip xs (tail xs)))

myRemoveDups :: Ord a => [a] -> [a]
myRemoveDups [] = []
myRemoveDups xs = map fst (sortBy snd (myRemoveDupsOrdBy fst (sortBy id (zip xs [0..]))))


-- Esercizio 2.1
-- ZipWith:: (a -> b -> c) -> [a] -> [b] -> [c]
-- applichiamo a [a] f quindi avremmo il tipo [b -> c] -> [b] -> [c] 
-- myZipWith f xs ys = zapp (map (f) xs) ys

-- Esercizio 2.2
myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f xs ys = map (uncurry f) (zip xs ys)

-- Esercizio 2.3
myMap :: (a -> b) -> [a] -> [b]
myMap f xs = foldr (\x acc -> f x : acc) [] xs

-- myMap :: (a -> b) -> [a] -> [b]
-- myMap f xs = foldl (\acc x -> acc ++ [f x]) [] xs

-- Esercizio 2.4
-- fold* non puo' esser sostituita da map poiche'
-- fold* cambia i valori dispetto ai precedenti, mentre
-- map non puo', un esempio e' dove definiamo 
-- L[i] come la somma dei primi i numeri della lista
-- che puo' esser fatto in foldl ma non puo
-- esser fatto da map, a causa del tipo considerando
-- che in map sara' una lista di tipo b mentre in fold*
-- ci aspettiamo un tipo b generico che puo' essere anche una lista

-- Esercizio 3.1
prefixes :: [a] -> [[a]]
prefixes [] = []
prefixes (x:xs) = map (x :) ([] : prefixes xs)

-- Esercizio 3.2
allConsSubLists :: [a] -> [[a]]
allConsSubLists [] = [[]]
allConsSubLists (x:xs) = prefixes (x:xs) ++ allConsSubLists xs 

segSommaS :: (Eq a, Num a) => [a] -> a -> [[a]]
segSommaS [] _ = []
segSommaS xs s= filter((==s) . sum) (allConsSubLists xs)


-- Esercizio 3.3
allSubLists :: [a] -> [[a]]
allSubLists [] = [[]]
allSubLists (x:xs) = (map (x:) res) ++ res
  where res = allSubLists xs

sublSommaS :: (Eq a, Num a) => [a] -> a -> [[a]]
sublSommaS [] _ = []
sublSommaS xs s = filter ((==s) . sum) (allSubLists xs)


-- Esercizio 4.1
-- avremmo una tabbella nxn dove T[i, j] = il numero
-- di modi di avere un array con all'inizio i e dopo
-- tutti numeri maggiori uguali e con somma dell'array j
emptyMatrix :: Int -> [[a]]
emptyMatrix n = [[] | _ <- [0..n]]

appendMat :: Num a => [[a]] -> Int -> a -> [[a]]
appendMat (xs:xss) 1 val = (xs ++ [val]) : xss
appendMat (xs:xss) i val = xs : appendMat xss (i-1) val

takeRow :: Int -> [[a]] -> [a]
takeRow 0 (xs:xss) = xs
takeRow n (xs:xss) = takeRow (n-1) xss

skipNextN :: Int -> [a] -> [a]
skipNextN _ [] = []
skipNextN 0 xs = xs
skipNextN n (x:xs) = skipNextN (n-1) xs

takeTail :: Int -> Int -> [[a]] -> [a]
takeTail r c xss = skipNextN c xs
  where xs = takeRow r xss

generateMatrix :: Num a => Int -> Int -> Int -> [[a]] -> [[a]]
generateMatrix i j n matrix
  | i == j && i == n = appendMat matrix i 1
  | i == j = generateMatrix (i + 1) 1 n (appendMat matrix i 1)
  | otherwise = generateMatrix (i) (j + 1) n (appendMat matrix i (sum (takeTail (i - j -1) (j-1) matrix)))

part :: Int -> Integer
part n = sum (takeRow (n-1) (generateMatrix 1 1 n (emptyMatrix n)))


-- Esercizio 4.2
-- costo n^2
-- la soluzione si risolve con dp e assumendo che 
-- ci sia un array dove T[i] = il numero di array con
-- somma uguale i quindi T[i] = sum(T[0: i]) (i escluso)
-- un altro modo sarebbe ricordarsi la somma e moltiplicarla per 2
-- ma vedremmo un modo piu' efficiente
mkpart' :: [Integer] -> Int -> Integer
mkpart' xs n
  | n == 0 = sum xs
  | otherwise = mkpart' (sum xs : xs) (n-1)

part' :: Int -> Integer
part' 0 = 1
part' n = mkpart' [1] (n-1)

-- lo possiamo anche fare in log n
-- basta dimostrare che part' n = 2^(n-1) per n > 0
power :: Integer -> Integer -> Integer
power _ 0 = 1
power 0 _ = 0
power 1 _ = 1
power x n = pref * hpower * hpower 
            where 
              pref = if odd n then x else 1 
              hpow = if odd n then (n-1) `div` 2 else n `div` 2
              hpower = power x hpow

part'' :: Integer -> Integer
part'' 0 = 1
part'' n = power 2 (n-1)


-- Esercizio 4.3
mkComb ys currents n counter
  | n == counter = [currents]
  | counter > n = []
  | null ys = []
  | otherwise = mkComb (tail ys) currents n counter ++ mkComb ys ((head ys) : currents) n ((head ys) + counter)

parts n = mkComb [1..(n+1)] [] n 0

-- il costo del 4.3 ci costa O(2^n * n) mentre il 4.1 ci puo' costare con ottimizzazioni
-- n^2 ma in haskell penso solo n^3 considerando l'accesso alle liste in tempo non costante e sarebbe anche il costo del algoritmo
-- considerando per per all'incirca n^2 celle, per ognuna
-- dobbiamo fare n operazioni