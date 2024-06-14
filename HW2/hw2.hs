main :: IO ()
main = print 2002688

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge xs@(x:txs) ys@(y:tys) 
 | x < y = x : merge txs ys
 | otherwise = y : merge xs tys
  
-- Esercizio 1.1
myMergeSort :: Ord a => [a] -> [a]
myMergeSort [] = []
myMergeSort xs = head (mSort (map singleton xs))
  where 
    singleton x = [x]
    mSort [] = []
    mSort (xs:xss) 
      | null xss = [xs]
      | otherwise = mSort (merge xs (head xss) : (mSort (tail xss)))

-- Esercizio 1.2 
-- basta migliorare il caso in cui la lista e' ordinata ma tanto che ci siamo
-- possiamo anche farlo quando e' ordinata al contrario e quindi possiamo
-- creare delle seguenze che sono ordinate e crearle in base al fatto che siano o meno
-- crescenti, questo porta a un caso miglioramento nullo nel caso peggiore cioe' avremmo solo
-- coppie di 2, mentre nel caso migliore abbimo un costo lineare, il costo sarebbe
-- nlogk dove k e' il numero di seguenze dopo sequences (se k = 1 allora abbiamo costo n e non consideriamo il k)
myMergeSort' :: Ord a => [a] -> [a]
myMergeSort' [] = []
myMergeSort' xs =  head (mSort (sequences xs))
  where
    sequences [] = []
    sequences (x: y : xs)
      | x > y = desc y (y:x:[]) xs
      | otherwise = asc y (\ys -> x:y:ys) xs
    sequences [x] = [[x]]
    
    desc lst curr xs@(x:txs)
      | x <= lst = desc x (x : curr) txs
      | otherwise = curr : sequences xs
    desc lst curr [] = [curr]
    
    asc lst curr xs@(x:txs)
      | x >= lst = asc x (\ys -> curr (x:ys)) txs
      | otherwise = (curr []) : sequences xs
    asc lst curr [] = [(curr [])]
      
    mSort [] = []
    mSort (xs:xss) 
      | null xss = [xs]
      | otherwise = mSort (merge xs (head xss) : (mSort (tail xss)))

-- Esercizio 2.1
data BinTree a = Node a (BinTree a) (BinTree a) | Empty
data BinTree' a = Node' (BinTree' a) (BinTree' a) | Leaf a

mapBT :: (a -> b) -> BinTree a -> BinTree b
mapBT f Empty = Empty
mapBT f (Node x l r) = Node (f x) (mapBT f l) (mapBT f r)

mapBT' :: (a -> b) -> BinTree' a -> BinTree' b
mapBT' f (Leaf x) = Leaf (f x)
mapBT' f (Node' l r) = Node' (mapBT' f l) (mapBT' f r)

-- foldl funziona come una sorta di visita in order che si calcola
-- il suo valore appena visita il nodo
foldlBT :: (a -> b -> b) -> b -> BinTree a -> b
foldlBT f z Empty = z
foldlBT f z (Node x l r) = foldlBT f (foldlBT f (f x z) l) r

-- il BT' avra' una funzione che dato il vecchio b fara' qualcosa
-- questo al fine di poter fare diverse cose in piu' rispetto a non averla
foldlBT' :: (a -> b -> b) -> (b -> b) -> b -> BinTree' a -> b
foldlBT' f g z (Leaf x) = f x z
foldlBT' f g z (Node' l r) = foldlBT' f g (foldlBT' f g (g z) l) r

-- foldr si camcolera' come ultimo il nodo che visita per prima 
-- e considerando che e' un binary tree avra' du e valori di ritorno
foldrBT :: (a -> b -> b -> b) -> b -> BinTree a -> b
foldrBT f z Empty = z
foldrBT f z (Node x l r) = f x (foldrBT f z l) (foldrBT f z r)


foldrBT' :: (a -> b -> b) -> (b -> b -> b) -> b -> BinTree' a -> b
foldrBT' f g z (Leaf x) = f x z
foldrBT' f g z (Node' l r) = g (foldrBT' f g z l) (foldrBT' f g z r)

-- Esercizio Bonus, Implementare sortBT' :: Ord a => BinTree' a -> [a]
sortBT' :: Ord a => BinTree' a -> [a]
sortBT' t = foldrBT' (\x y -> x:y) merge [] t


-- Esercizio 2.2
numNodes :: BinTree a -> Int
numNodes = foldrBT (\x y z -> 1 + y + z) 0

numNodes' :: BinTree' a -> Int
numNodes' = foldrBT' (\x y -> 1) (\y z -> 1 + y + z) 0

height :: BinTree a -> Int
height t = max ((foldrBT (\x y z -> 1 + max y z) 0 t) -1) 0

height' :: BinTree' a -> Int
height' t = max ((foldrBT' (\x y -> 1) (\y z -> 1 + max y z) 0 t) -1) 0

-- ritorniamo nelle chiamate dell'albero (max alteza, max sbilanciamento)
-- dove l'attuale indice di sbilanciamento e' altezza sottAlbD - sottAlbS,
-- mentre il massimo e' il massimo dell'idice di sbilanciamentro tra
-- l'attuale e quello di destra e sinistra
maxImbalance :: BinTree a -> Int
maxImbalance t = snd (foldrBT (\x (hy, ry) (hz, rz) -> (1 + max hy hz, max ry (max (abs (hy - hz)) rz))) (0, 0) t)

maxImbalance' :: BinTree' a -> Int
maxImbalance' t = snd (foldrBT' (\x y -> (0, 0)) (\(hy, ry) (hz, rz) -> (1 + max hy hz, max ry (max (abs (hy - hz)) rz))) (0, 0) t)


 -- facoltativo
data Tree a = R a [Tree a]

mapT :: Tree a -> (a -> b) -> Tree b
mapT (R a xs) f = R (f a) [mapT x f | x <- xs]

foldlT :: (a -> b -> b) -> b -> Tree a -> b
foldlT f b (R a xs) = foldl (foldlT f) (f a b) xs

myFoldl :: (b -> b -> b) -> b -> [b] -> b
myFoldl f b [] = b
myFoldl f b [x] = x
myFoldl f b (x:xs) = f x (myFoldl f b xs)

-- l'effetivo caso base ce lo calcoliamo solo per le foglie
foldrT :: (a -> b -> b) -> b -> (b -> b -> b) -> Tree a -> b
foldrT f b g (R a xs) = f a (myFoldl g b [foldrT f b g x | x <- xs])

numNodesT :: Tree a -> Int
numNodesT = foldrT (\_ x -> 1 + x) 0 (\x y -> x + y)

heightT :: Tree a -> Int
heightT t = (foldrT (\_ x -> 1 + x) 0 max t) -1

fstEl :: (a, b, c) -> a
fstEl (x, _, _) = x

sndEl :: (a, b, c) -> b
sndEl (_, y, _) = y

thrEl :: (a, b, c) -> c
thrEl (_, _, z) = z

maxTriple :: (Ord a, Ord b, Ord c) => (a, b, c) -> (a, b, c) -> (a, b, c)
maxTriple (x1, x2, x3) (y1, y2, y3) = (max x1 y1, min x2 y2, max x3 y3)

maxImbalanceT :: Tree a -> Int
maxImbalanceT t = thrEl (foldrT (\_ res -> (1 + fstEl res, 1 + sndEl res, max (thrEl res) (fstEl res - sndEl res))) (0, 0, 0) maxTriple t)

-- Esercizio 3
-- ogni volta ritorniamo il res dell' albero e la somma dell' albero
-- compresa la radice, e poi andiamo a confrontare con sumToRoot che 
-- sarebbe la somma dei valori del cammino dalla radice iniziale 
-- all'attuale nodo non comprendendo il valore dell'attuale nodo
nodiEquilibrati:: (Num a, Eq a) => BinTree a -> [a]
nodiEquilibrati t = fst (nodiEquilibrati' t 0)
  where nodiEquilibrati' Empty sumToRoot = ([], 0)
        nodiEquilibrati'  (Node x l r) sumToRoot = if subTreeSum == sumToRoot then (x : res, subTreeSum) else (res, subTreeSum)
          where 
            (lRes, lSubSum) = nodiEquilibrati' l (sumToRoot + x)
            (rRes, rSubSum) = nodiEquilibrati' r (sumToRoot + x)
            res = lRes ++ rRes
            subTreeSum = lSubSum + rSubSum + x


-- Esercizio 4
-- il fatto di metterli uno dopo l'altro ci potrebbe costare O(n) nell'inserimento
-- quindi rendendo il nostro albero bilanciato ci mettiamo O(n logn) mettendo
-- ogni volta il numero al centro (di un array sortato) come radice e mettere
-- come sottoalberi la parte sinistra e la parte destra rispetto al centrale
myRemoveDupsOrd :: Ord a => [a] -> [a]
myRemoveDupsOrd [] = []
myRemoveDupsOrd xs = head xs : map (snd) (filter (uncurry (/=)) (zip xs (tail xs)))

listToBST :: Ord a => [a] -> BinTree a
listToBST = listToBST' . myRemoveDupsOrd . myMergeSort
  where 
    listToBST' [] = Empty
    listToBST' xs = Node (head right) (listToBST' left) (listToBST' (tail right))
      where 
        (left, right) = splitAt ((length xs -1) `div` 2) xs


-- Esercizio 5
-- scanr f e = map (foldr f e) . tails
-- abbiamo che dato una lista [a_1, ..., a_n] ora dimostriamo che 
-- la funzione scanr e' uguale a myScanr (n lunghezza della lista)
-- 
-- Caso base n = 0 => e in entrambi i casi
--
-- Ipotesi Induttica per i
-- scanr f e = myScanr f e ([a_1, ..., a_(i -1)])
-- 
-- Passo Induttivo
-- scanr f e ?= myScanr f e ([a_1, ..., a_i])
-- sappiamo che tails ritorna tutti i prefissi e sappiamo che 
-- tails (x:xs) = [x:xs] ++ tails xs
-- quindi sappiamo che sicuramente il primo res di myScanr e' 
-- giusto per ipotesi induttiva ora dobbiamo dimostrare che
-- f x (head res) = foldr f e (x:xs) che sarebbe f x_1 (f ... f x_i e)
-- e sappiamo che (f ... e) non e' altro che il risultato di foldr f e xs
-- il che per ipotesi induttiva e' l'head di res e quindi f x (head res) = foldr f e (x:xs)
myScanr :: (a -> b -> b) -> b -> [a] -> [b]
myScanr f e [] = [e]
myScanr f e (x:xs) = f x (head res) : res
  where res = myScanr f e xs
