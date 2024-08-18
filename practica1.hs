--Practica 1 : Programación Funcional 
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
--esto esta como el orto
permutaciones :: (Eq t) => [t] -> [t]
permutaciones [] = [[]]
permutaciones (x:xs) = concatMap (permutaciones (drop long xs)) [x]
    where long = length xs

