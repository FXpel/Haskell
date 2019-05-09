module Main where

import Graphics.Gloss

main = animate (InWindow "Dragon" (500, 500) (0, 0)) white (dragonAnime (50,250) (450,250))


dragon :: Point -> Point -> [Path]
dragon x y = iterate pasDragon([x,y])




pointAintercaler :: Point -> Point -> Point
pointAintercaler (xA,yA) (xB,yB) = ((xA + xB)/2 + (yB - yA)/2, (yA + yB)/2 + (xA - xB)/2)


pasDragon :: Path -> Path
pasDragon([]) = []
pasDragon([x])= [x]
pasDragon (x:xs:xss) = x : (pointAintercaler x xs) :pasDragon'(xs:xss)

pasDragon' :: Path -> Path
pasDragon'([]) = []
pasDragon'([x]) = [x]
pasDragon' (x:xs:xss) = x : (pointAintercaler xs x) : pasDragon(xs:xss)
--Q7:
--dragonAnime a b t = Line (dragon a b !! (round t `mod` 20))


dragonOrdre :: Point -> Point -> Int -> Path
dragonOrdre x y 0 = [x,y]
dragonOrdre x y n = pasDragon(dragonOrdre x y (n-1))

--Q9:
dragonAnime a b t = Line (dragonOrdre a b (round t `mod` 20))
