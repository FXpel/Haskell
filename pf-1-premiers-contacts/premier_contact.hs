sommeDeXaY :: Int -> Int -> Int
sommeDeXaY x y = sum [x..y]

somme ::[Int]-> Int
somme [] = 0
somme (x:xs) = x + somme xs

last' :: [a]->a
last' [x]=x
last' (x:xs) = last(tail xs)

init' :: [a]->[a]
init' [x] = []
init'(x:xs) = x : init xs

index :: [a] -> Int -> a
index (x:xs) 0 = x
index (x:xs) n = index xs (n-1)

concat' :: [a] -> [a] -> [a]
concat' [] l = l
concat' [x] l = x : l
concat' (x:xs) l = x:(concat' xs l)

concatlistes :: [[a]] -> [a]
concatlistes [] = []
concatlistes (x:xs) = concat' x (concatlistes xs)

mymap :: (a->b) -> [a] -> [b]
mymap f [] = []
mymap f (x:xs) = (f x) : (mymap f xs)


--la déclaration noté comme ça donnerai une fonction partielle

longueurliste :: [a] -> Int
longueurliste l = somme (map (\x->1) l)

listefof :: (a->a)->a -> Int-> [a]
listefof f x n | n < 0 = []
listefof f x n = x :(listefof f (f x) (n-1))

listefof' :: (a->a)->a -> Int-> [a]
listefof' f x n = take n (iterate f x)

listeentier :: Int ->[Int]
listeentier n = listefof (\x->x+1) 0 (n)
