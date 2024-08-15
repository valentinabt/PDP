--Pre-práctica de Programación Funcional 

--2.
--a. 
valorAbsoluto :: Float -> Float 
valorAbsoluto x | x>=0 = x
                | otherwise = -x 

--b
bisiesto :: Integer -> Bool 
bisiesto año | mod año 4 == 0 && (mod año 100 /= 0 || (mod año 100 == 0 && mod año 400 == 0)) = True
             | otherwise = False 

--c
factorial :: Integer -> Integer
factorial n | n== 1 = n 
            | otherwise = n* factorial (n-1)

--d
--ni ganas de pensar 

--3)

inverso :: Float -> Maybe Float
inverso n
    | n == 0    = Nothing
    | otherwise = Just (1 / n)
                      
aEntero :: Either Int Bool -> Int
aEntero (Left n) = n 
aEntero (Right n) | n== True = 1 
                  | n == False = 0 

--4)

pertenece :: (Eq t) => t -> [t] -> Bool 
pertenece x [] = False
pertenece x (y:ys) | x== y = True
                   | otherwise = pertenece x ys 




limpiar :: [Char] -> [Char] -> [Char] 
limpiar (x:xs) [] = []
limpiar (x:xs) (y:ys) | x == y && pertenece x ys = limpiar (x:xs) ys 
                      | x == y && not (pertenece x ys) = limpiar xs ys 
                      | x /= y && pertenece x ys = [y] ++ limpiar (x:xs) ys 
                      | otherwise = [y] ++ limpiar xs ys 
--esta mal,hay que hacer doble recursion creo o capaz esta mal el caso base ni idea 

--b)

sumatoria :: [Float] -> Float 
sumatoria (x:[]) = x 
sumatoria (x:xs) = x + sumatoria xs 

longitud :: (Eq t) => [t] -> Float
longitud [] = 0 
longitud (x:xs) = 1 + longitud xs

promedio :: [Float] -> Float 
promedio xs = sumatoria xs / (longitud xs)


difPromedio :: [Float] -> [Float]
difPromedio xs = difPromedio1 xs sum 
 where sum = promedio xs 

difPromedio1 :: [Float] -> Float -> [Float]
difPromedio1 [x] y = [x-y]
difPromedio1 (x:xs) sum = [x-sum] ++ difPromedio1 xs sum  


--c)
todosIguales :: [Int] -> Bool 
todosIguales (x:[]) = True
todosIguales (x:y:xs) | x /= y = False 
                      | otherwise = todosIguales (y:xs)


--4)
data AB a = Nil | Bin (AB a) a (AB a)

avacioAB :: AB a -> Bool
avacioAB Nil = True
avacioAB _ = False