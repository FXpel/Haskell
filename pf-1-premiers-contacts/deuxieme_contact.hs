alterne :: [a] -> [a]
alterne [] = []
alterne [x] = [x]
alterne (x1:x2:xs) = [x1] ++ alterne(xs)

combine ::(a -> b -> c) -> [a] -> [b] -> [c]
combine f [] _ = []
combine f _ [] = []
combine f [x] [y] = [f x y]
combine f (x:xs) (y:ys) = (f x y) : (combine f xs ys)

pasPascal :: [Integer] -> [Integer]
pasPascal []=[]
pasPascal [x]= 1:[x]
pasPascal (x:xs)= 1: (zipWith (+) (x:xs) xs)++[1]

pascal :: [[Integer]]
pascal = iterate (pasPascal) [1]
