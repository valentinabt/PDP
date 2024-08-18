--Practica 1 : Programación Funcional 
--2)
--curry :: ((a->b)->c)->(a->b->c)
--curry f (x,y) = \x y -> f (x,y)

--unCurry :: (a->b->c) -> (a->b)->c
--unCurry f a b = \(a,b) -> f a b 
--a chequear 
--3)
--I) 
sum :: Num a => [a] -> a 
sum (x:[]) = x
sum xs = foldr (+) (last xs) (init xs)

--II)
--elem1 :: (Eq t) => [t] -> t -> Bool
--elem1 [] x = False 
--elem xs x = foldr (\f ys y->if y==x then True else xs) False xs 
--III)
concaTnuev :: (Eq t) => [t] -> [t] -> [t]
concaTnuev [x] [y] = [x,y]
concaTnuev xs ys = foldr (:) ys xs 

--IV)
filter1 :: (a->Bool) -> [a] -> [a]
filter1 p xs = foldr (\x ys -> if p x then x: ys else ys) [] xs

--V)
map1 :: (a->b) -> [a] -> [b]
map1 f xs = foldr (\x  ys -> f x : ys) [] xs 

--4)
--IIV) 
sumaParcial :: Num a => [a] -> a -> [a]
sumaParcial (x:[]) n = [x+n]
sumaParcial (x:xs) n = [x+n] ++ sumaParcial xs (n+x)

sumasParciales :: Num a => [a] -> [a]
sumasParciales xs = sumaParcial xs 0 




--IV) 
alternada :: [Float] -> Float -> Float
alternada [] n = n 
alternada (x:[]) n = x+n 
alternada (x:y:[]) n = n+x-y
alternada (x:y:xs) n = alternada xs (n+x-y)

sumaAlternada :: [Float] -> Float 
sumaAlternada xs = alternada xs 0 

--5)
--preguntar 
permutaciones :: (Eq t) => [t] -> [t]
permutaciones [] = []


