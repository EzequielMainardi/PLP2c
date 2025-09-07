import Control.Arrow (Arrow(first))

max2 :: Ord a => (a,a) -> a
max2 (x, y) | x >= y = x
              | otherwise = y

normaVectorial:: (Float,Float) -> Float
normaVectorial (x, y) = sqrt (x^2 + y^2)

subtract:: Num a => a->a->a 
subtract = flip (-) --flip :: (a->b->c) -> (b-> a ->c) 

predecesor :: Num a => a->a 
predecesor = Prelude.subtract 1 


evaluarEnCero :: Num a => (a->b) -> b
evaluarEnCero = \f -> f 0

dosVeces :: (a -> a) -> a -> a
dosVeces = \f -> f . f

flipAll :: [a -> b -> c] -> [b -> a -> c]
flipAll = map flip
 
flipRaro :: b -> (a -> b -> c) -> a -> c
flipRaro = flip flip

curry:: ((a,b)->c) -> (a->b->c)
curry f x y = f (x, y)

uncurry:: (a->b->c) -> ((a,b) -> c)
uncurry f (x,y) = f x y

--curryNormaVectorial :: Float->Float->Float
--curryNormaVectorial = Main.curry normaVectorial
sumFoldr::(Foldable t, Num a) => t a -> a
sumFoldr = foldr (+) 0

elemFoldr:: (Foldable t, Eq a) => a -> t a -> Bool
elemFoldr x = foldr (\y r -> x==y || r) False

concatFoldr:: [[a]] -> [a]
concatFoldr = foldr (\xs acc ->  (++) xs acc)  []

--sumasParciales :: Num a => [a] -> [a]
--sumasParciales  = foldl f []
--        where f x = 

sumaAlt :: Num a =>[a] -> a
sumaAlt = foldr (-) 0 

sumaAlt2 :: Num a => [a] -> a 
sumaAlt2 = foldl (flip (-)) 0

--permutaciones :: [a] -> [[a]]
--permutaciones = foldr (\x r -> concatMap (f ))