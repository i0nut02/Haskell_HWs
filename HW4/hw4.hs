import Control.Monad.State
import Control.Applicative

main :: IO ()
main = print "2002688"

-- Esercizio 1
toLowercase :: Char -> Char
toLowercase c
  | 'A' <= c && c <= 'Z' = toEnum (fromEnum c + 32)
  | otherwise = c

-- Inserisce un elemento e ne elimina le occorenze in xs
nub :: Eq a => [a] -> [a]
nub [] = []
nub (x:xs) = x : nub (filter (/= x) xs)

-- Inserisce la coppia chiave valore o aggiorna il valore della chiave
updateMap :: Eq k => [(k, v)] -> k -> v -> (v -> v -> v) -> [(k, v)]
updateMap [] key val _ = [(key, val)]
updateMap ((k, v):xs) key val f
  | k == key  = (k, f v val) : xs
  | otherwise = (k, v) : updateMap xs key val f

-- ritorna in quante stringhe di input un carattere e' presente 
countChars :: [String] -> [(Char, Int)]
countChars strs = foldl countInStr [] strs
  where
    countInStr acc str = foldl (\m c -> updateMap m c 1 (+)) acc (nub (map toLowercase str))

charCount :: IO ()
charCount = do
    putStrLn "Inserisci il numero di stringhe da scrivere"
    n <- readLn

    putStrLn $ "Inserisci " ++ show n ++ " stringhe:"
    strings <- sequence (replicate n getLine)

    let charCounts = countChars strings

    mapM_ (\(char, count) -> putStrLn $ "Il carattere '" ++ [char] ++ "' appare in " ++ show count ++ " stringhe") charCounts


-- Esercizio 2
data BinTree a = Node a (BinTree a) (BinTree a) | Empty
  deriving (Show, Eq)

updateFirst val = state (\(n1, n2) -> (n1, (n1 + val, n2)))

append val = state (\(n1, n2) -> (n1, (n1, val : n2)))

takeAndSaveSubTree val = state (\(n1, n2) -> (val + sum (take 2 n2), (n1, val + sum (take 2 n2) : (drop 2 n2))))

-- Il problema principale di questo esercizio e' fare Applicative e 
-- un modo diretto per farlo sarebbe di non utilizzare variabili di
-- appoggio in balancedNodesM e che modificano lo stato, ad esempio
-- qui si poteva usare la seconda variabile di stato per salvarsi la
-- singola somma del albero attuale, e per farlo bisogna salvarsi la 
-- somma del sottoalbero sinistro per poi poter visitare la parte desta
-- e ritrovarsi la somma del sottoalbero destro nella seconda variabile.
-- 
-- Una soluzione e' salvarsi il valore del sottoalbero sinistro e destro
-- insieme, e grazie alla ricorsio che si basa sullo stack, si puo
-- creare uno stack che cresce con le chiamate ricorsive e diminuisce
-- quando finiscono, quindi le foglie di default ritornerrano 0 e i
-- nodi sopra leggono i risultati dei propi figli nello stack salvato
-- nello stato e ci aggiunge il suo valore attuale, poi come primo
-- valore dello stato c'e' il cammino dalla radice al nodo attuale
-- e basta aggiungere il valore del nodo quando entriamo nella chiamate
-- e basta sottrarre il valore del nodo quando usciamo dalla chiamata
-- perche' ritorna nel padre e sicuramente non fara' parte del prossimo
-- cammino in quanto i suoi figli sono stati gia' visitati
balancedNodesM b = evalState (balancedNodesM' b) (0, [])
  where
    balancedNodesM' Empty = do 
      _ <- append 0
      return []
    balancedNodesM' (Node val left right) = do 
      n <- updateFirst val
      lres <- balancedNodesM' left
      rres <- balancedNodesM' right
      totSubSum <- takeAndSaveSubTree val
      _ <- updateFirst (-val)
      return (if n == totSubSum then val: lres ++ rres else lres ++ rres)

-- considerando che non ci sono dipendenze possiamo
-- scrivere la funzione passando in input i valori
-- e farlo nell'ordine sopra
balancedNodesA b = evalState (balancedNodesA' b) (0, [])
  where
    balancedNodesA' Empty = append 0 *> pure []
    balancedNodesA' (Node val left right) = 
      (\n lres rres totSubSum _ -> (if n == totSubSum then val: lres ++ rres else lres ++ rres))
        <$> updateFirst val
        <*> balancedNodesA' left
        <*> balancedNodesA' right
        <*> takeAndSaveSubTree val
        <*> updateFirst (-val)


-- Esercizio 3
data NatBin = End | Zero NatBin | One NatBin deriving (Show, Eq)

data Term = Value NatBin 
          | Add Term Term 
          | Sub Term Term 
          | Mul Term Term 
          | Div Term Term 
          | Mod Term Term
          deriving (Show, Eq)

data MaybeTerm a = JustTerm a | NegativeResult | DivisionByZero | Overflow
                 deriving (Show, Eq)

-- Bisogna eliminare i zeri iniziali e poi bisogna
-- confrontare i due numeri basandosi su stessi
-- "indici" e confrontare i valori dal piu' 
-- significativo al meno, e se il risultato gia e'
-- deciso allora lo propaghiamo
instance Ord NatBin where
    compare a b = fst $ compare' (reverseNatBin (removeFirstsZeros (reverseNatBin a))) (reverseNatBin (removeFirstsZeros (reverseNatBin b)))
      where
        compare' :: NatBin -> NatBin -> (Ordering, Bool)
        compare' End End = (EQ, False)
        compare' End _ = (LT, True)
        compare' _ End = (GT, True)
        compare' (Zero nb1) (Zero nb2) = compare' nb1 nb2
        compare' (One nb1) (One nb2) = compare' nb1 nb2
        compare' (Zero nb1) (One nb2) = case compare' nb1 nb2 of
                                          (res, True) -> (res, True)
                                          (_, False) -> (LT, True)
        compare' (One nb1) (Zero nb2) = case compare' nb1 nb2 of
                                          (res, True) -> (res, True)
                                          (_, False) -> (GT, True)

removeFirstsZeros :: NatBin -> NatBin
removeFirstsZeros End = End
removeFirstsZeros (Zero nb) = removeFirstsZeros nb
removeFirstsZeros nb = nb

reverseNatBin :: NatBin -> NatBin
reverseNatBin = reverseNatBin' End
  where
    reverseNatBin' acc End = acc
    reverseNatBin' acc (Zero nb) = reverseNatBin' (Zero acc) nb
    reverseNatBin' acc (One nb) = reverseNatBin' (One acc) nb

instance Functor MaybeTerm where
    fmap _ Overflow = Overflow
    fmap _ NegativeResult = NegativeResult
    fmap _ DivisionByZero = DivisionByZero
    fmap f (JustTerm a) = JustTerm (f a)

instance Applicative MaybeTerm where
    pure = JustTerm
    Overflow <*> _ = Overflow
    NegativeResult <*> _ = NegativeResult
    DivisionByZero <*> _ = DivisionByZero
    JustTerm f <*> JustTerm a = JustTerm (f a)

instance Monad MaybeTerm where
    return = pure
    Overflow >>= _ = Overflow
    NegativeResult >>= _ = NegativeResult
    DivisionByZero >>= _ = DivisionByZero
    JustTerm a >>= f = f a


addNatBin :: NatBin -> NatBin -> MaybeTerm NatBin
addNatBin a b = addNatBin' a b End
  where
    addNatBin' End End End = JustTerm End
    addNatBin' End End carry = JustTerm carry
    addNatBin' End b carry = addNatBin' b End carry
    addNatBin' (Zero a) End (One End) = fmap One (addNatBin' a End End)
    addNatBin' (Zero a) End End = fmap Zero (addNatBin' a End End)
    addNatBin' (One a) End (One End) = fmap Zero (addNatBin' a End (One End))
    addNatBin' (One a) End End = fmap One (addNatBin' a End End)
    addNatBin' (Zero a) (Zero b) End = fmap Zero (addNatBin' a b End)
    addNatBin' (Zero a) (One b) End = fmap One (addNatBin' a b End)
    addNatBin' (One a) (Zero b) End = fmap One (addNatBin' a b End)
    addNatBin' (One a) (One b) End = fmap Zero (addNatBin' a b (One End))
    addNatBin' (Zero a) (Zero b) (Zero carry) = fmap Zero (addNatBin' a b carry)
    addNatBin' (Zero a) (Zero b) (One carry) = fmap One (addNatBin' a b carry)
    addNatBin' (Zero a) (One b) (Zero carry) = fmap One (addNatBin' a b carry)
    addNatBin' (Zero a) (One b) (One carry) = fmap Zero (addNatBin' a b (One carry))
    addNatBin' (One a) (Zero b) (Zero carry) = fmap One (addNatBin' a b carry)
    addNatBin' (One a) (Zero b) (One carry) = fmap Zero (addNatBin' a b (One carry))
    addNatBin' (One a) (One b) (Zero carry) = fmap Zero (addNatBin' a b (One carry))
    addNatBin' (One a) (One b) (One carry) = fmap One (addNatBin' a b (One carry))

subNatBin :: NatBin -> NatBin -> MaybeTerm NatBin
subNatBin a b = if a < b then NegativeResult else subNatBin' a b (Zero End)
  where
    subNatBin' :: NatBin -> NatBin -> NatBin -> MaybeTerm NatBin
    subNatBin' End End (Zero End) = JustTerm End
    subNatBin' (Zero a) End (Zero End) = fmap Zero (subNatBin' a End (Zero End))
    subNatBin' (Zero a) End (One End) = fmap One (subNatBin' a End (One End))
    subNatBin' (One a) End (Zero End) = fmap One (subNatBin' a End (Zero End))
    subNatBin' (One a) End (One End) = fmap Zero (subNatBin' a End (Zero End))
    subNatBin' End (Zero b) (Zero End) = fmap Zero (subNatBin' End b (Zero End))
    subNatBin' End (Zero b) (One End) = fmap One (subNatBin' End b (One End))
    subNatBin' End (One b) (Zero End) = fmap One (subNatBin' End b (One End))
    subNatBin' End (One b) (One End) = fmap Zero (subNatBin' End b (One End))
    subNatBin' (Zero a) (Zero b) (Zero End) = fmap Zero (subNatBin' a b (Zero End))
    subNatBin' (Zero a) (Zero b) (One End) = fmap One (subNatBin' a b (One End))
    subNatBin' (Zero a) (One b) (Zero End) = fmap One (subNatBin' a b (One End))
    subNatBin' (Zero a) (One b) (One End) = fmap Zero (subNatBin' a b (One End))
    subNatBin' (One a) (Zero b) (Zero End) = fmap One (subNatBin' a b (Zero End))
    subNatBin' (One a) (Zero b) (One End) = fmap Zero (subNatBin' a b (Zero End))
    subNatBin' (One a) (One b) (Zero End) = fmap Zero (subNatBin' a b (Zero End))
    subNatBin' (One a) (One b) (One End) = fmap One (subNatBin' a b (One End))
    subNatBin' _ _ End = JustTerm End
    subNatBin' _ _ _ = NegativeResult

- La moltiplicazione e' la somma di a con se stesso per b volte 
mulNatBin :: NatBin -> NatBin -> MaybeTerm NatBin
mulNatBin a b = mulNatBin' a b (Zero End)
  where
    mulNatBin' End _ acc = JustTerm acc
    mulNatBin' _ End acc = JustTerm acc
    mulNatBin' (One a) b acc = do
        newAcc <- addNatBin b acc
        mulNatBin' a (Zero b) newAcc
    mulNatBin' (Zero a) b acc = mulNatBin' a (Zero b) acc

-- cerchiamo di vedere quante volte possiamo sottrarre
-- b ad a (fino a quando non ritorna un risultato negativo)
-- e ritorniamo l'accumulatore
divNatBin :: NatBin -> NatBin -> MaybeTerm NatBin
divNatBin _ End = DivisionByZero
divNatBin End _ = JustTerm End
divNatBin a b = if b == (Zero End) then DivisionByZero else divNatBin' a b End
  where
    divNatBin' :: NatBin -> NatBin -> NatBin -> MaybeTerm NatBin
    divNatBin' a b acc = case subNatBin a b of
        NegativeResult -> JustTerm acc
        JustTerm diff -> do
            newAcc <- addNatBin acc (One End)
            divNatBin' diff b newAcc
        _ -> Overflow

-- quando a - b crea un valore negativo allora 
-- il resto/mod e' propio a, quindi basta sottrarre
-- ad a b fino a quando a - b e' negativo
modNatBin :: NatBin -> NatBin -> MaybeTerm NatBin
modNatBin _ End = DivisionByZero
modNatBin End _ = JustTerm End
modNatBin a b = if b == (Zero End) then DivisionByZero else modNatBin' a b
  where
    modNatBin' a b = case subNatBin a b of
        NegativeResult -> JustTerm a
        JustTerm diff -> modNatBin' diff b
        _ -> Overflow

-- il limite e' 8 bit, il 9 e' End e 10+ e' overflow
checkOverflow :: MaybeTerm NatBin -> MaybeTerm NatBin
checkOverflow (JustTerm nb) = checkOverflow' 1 nb
  where
    checkOverflow' 10 _ = Overflow
    checkOverflow' _ End = JustTerm End
    checkOverflow' n (One nb') = fmap One . checkOverflow' (n + 1) $ nb'
    checkOverflow' n (Zero nb') = fmap Zero . checkOverflow' (n + 1) $ nb'
checkOverflow x = x

eval :: Term -> MaybeTerm NatBin
eval (Value n) = checkOverflow (JustTerm n)
eval (Add x y) = do
    a <- eval x
    b <- eval y
    result <- addNatBin a b
    checkOverflow (JustTerm result)
eval (Sub x y) = do
    a <- eval x
    b <- eval y
    subNatBin a b
eval (Mul x y) = do
    a <- eval x
    b <- eval y
    mulNatBin a b
    result <- mulNatBin a b
    checkOverflow (JustTerm result)
eval (Div x y) = do
    a <- eval x
    b <- eval y
    divNatBin a b
eval (Mod x y) = do
    a <- eval x
    b <- eval y
    modNatBin a b
