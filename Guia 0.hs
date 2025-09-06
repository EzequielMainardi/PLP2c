import Data.ByteString.Builder (FloatFormat)

-- Guia 0

{- null :: [a] -> Bool -- Devuelve True si la lista está vacía, False en caso contrario.
head :: [a] -> a -- Devuelve el primer elemento de una lista. Provoca un error si la lista está vacía.
tail :: [a] -> [a] -- Devuelve todos los elementos de la lista excepto el primero. Error si la lista está vacía.
init :: [a] -> [a]-- Devuelve todos los elementos de la lista excepto el último. Error si la lista está vacía.
last :: [a] -> a-- Devuelve el último elemento de la lista. Error si la lista está vacía.
take :: Int -> [a] -> [a] --Toma los primeros n elementos de una lista. Si n es mayor que la longitud de la lista, devuelve toda la lista. Si n <= 0, devuelve lista vacía.
drop :: Int -> [a] -> [a]--Elimina los primeros n elementos de una lista. Si n es mayor que la longitud de la lista, devuelve lista vacía.
(++) :: [a] -> [a] -> [a]-- Concatena dos listas.
concat :: [[a]] -> [a]--Concatena una lista de listas en una sola lista.
reverse :: [a] -> [a]--Invierte el orden de los elementos de la lista.
elem :: Eq a => a -> [a] -> Bool --Devuelve True si el elemento está en la lista, False si no. Requiere que los elementos sean comparables por igualdad.
 -}

-- ej2

valorAbsoluto :: Float -> Float
valorAbsoluto n = if n < 0 then -n else n

bisiesto :: Int -> Bool
bisiesto n = (mod n 4 == 0 && mod n 100 /= 0 || mod n 400 == 0)

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- cantDivisoresPrimos :: Int -> Int
-- cantDivisoresPrimos n = length[x| x <-[2..n], mod n x == 0, esPrimo x ]

-- esPrimo :: Int -> Bool
-- esPrimo n | n < 2 = False
--          | otherwise = null [x| x <- [2..(n-1)], mod n x == 0]

inverso :: Float -> Maybe Float
inverso 0 = Nothing
inverso n = Just (1 / n)

aEntero :: Either Int Bool -> Int
aEntero (Left n) = n
aEntero (Right b) = if b then 1 else 0

limpiar :: [Char] -> [Char] -> [Char]
limpiar _ [] = []
limpiar xs (y : ys)
  | elem y xs = limpiar xs ys
  | otherwise = y : limpiar xs ys

difPromedio :: [Float] -> [Float]
difPromedio notas = map (\x -> x - promedio) notas
  where
    promedio = sum notas / fromIntegral (length notas)

todosIguales :: [Int] -> Bool
todosIguales [] = True
todosIguales [_] = True
todosIguales (x : y : xs)
  | x == y = todosIguales (x : xs)
  | otherwise = False

data AB a = Nil | Bin (AB a) a (AB a)
  deriving (Show)

vacio :: AB a -> Bool
vacio Nil = True
vacio _ = False

negacionAB :: AB Bool -> AB Bool
negacionAB Nil = Nil
negacionAB (Bin i a d) = Bin (negacionAB i) (not a) (negacionAB d)

productoAB :: AB Int -> Int
productoAB Nil = 1
productoAB (Bin i a d) = productoAB i * a * productoAB d
