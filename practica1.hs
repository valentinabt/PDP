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


elem1 :: (Eq t) => [t] -> t -> Bool
elem1 [] x = False 

elem1 xs x = foldr (\y acumulador-> x==y || acumulador) False xs 
--en acumulador voy guardando todas las comparaciones con elementos que ya comparé con x

concaTnuev :: (Eq t) => [t] -> [t] -> [t]
concaTnuev [x] [y] = [x,y]
concaTnuev xs ys = foldr (:) ys xs 


filter1 :: (a->Bool) -> [a] -> [a]
filter1 p xs = foldr (\x ys -> if p x then x: ys else ys) [] xs


map1 :: (a->b) -> [a] -> [b]
map1 f xs = foldr (\x  ys -> f x : ys) [] xs 

--II)
mejorSegún :: (a -> a -> Bool) -> [a] -> a
mejorSegún f xs = foldr1 (\x y -> if f x y then x else y) xs 

--III)

sumaParcial :: Num a => [a] -> a -> [a]
sumaParcial (x:[]) n = [x+n]
sumaParcial (x:xs) n = [x+n] ++ sumaParcial xs (n+x)

sumasParciales :: Num a => [a] -> [a]
sumasParciales xs = sumaParcial xs 0 

--IV) no use foldr xd 
alternada :: [Float] -> Float -> Float
alternada [] n = n 
alternada (x:[]) n = x+n 
alternada (x:y:[]) n = n+x-y
alternada (x:y:xs) n = alternada xs (n+x-y)

sumaAlternada :: [Float] -> Float 
sumaAlternada xs = alternada xs 0 

sumaAlternadaF :: [Float] -> Float
sumaAlternadaF [] = 0 
sumaAlternadaF (x:[]) = x 
sumaAlternadaF xs = foldr (\x y -> x-y) 0 xs

--V)
--no se si entendi la consigna o era facil
sumaAlternadaInv :: [Float] -> Float
sumaAlternadaInv [] = 0 
sumaAlternadaInv (x:[]) = x 
sumaAlternadaInv xs = foldr (\x y -> x-y) 0 xs



--5) re dificil 
--preguntar 
permutaciones :: (Eq t) => [t] -> [t]
permutaciones [] = []


