--import Test.QuickCheck
import Control.Concurrent (threadDelay)
main :: IO ()
main = undefined

data Arbre coul val = Feuille |Noeud coul val (Arbre coul val) (Arbre coul val)
    deriving (Show, Eq)

mapArbre :: (coul -> coul2)-> (val -> val2) ->  Arbre coul val -> Arbre coul2 val2
mapArbre fc fv Feuille = Feuille
mapArbre fc fv (Noeud c v a1 a2) = Noeud (fc c) (fv v) (mapArbre fc fv a1) (mapArbre fc fv a2)



hauteur :: Arbre coul val -> Int
hauteur Feuille = 0
hauteur (Noeud c v a1 a2) = 1 + max (hauteur a1) (hauteur a2)

taille :: Arbre coul val -> Int
taille Feuille = 0
taille (Noeud c v a1 a2) = 1 + (hauteur a1) + (hauteur a2)

dimension :: (Int -> Int -> Int)-> Arbre coul val -> Int
dimension f Feuille = 0
dimension f (Noeud c v a1 a2) =  1 + f (dimension f a1) (dimension f a2)

hauteur' :: Arbre coul val -> Int
hauteur' Feuille = 0
hauteur' a = dimension max a

taille' :: Arbre coul val -> Int
taille' Feuille = 0
taille' a = dimension (+) a


peigneGauche :: [(c,a)] -> Arbre c a
peigneGauche [] = Feuille
peigneGauche (h:t) = Noeud (fst h) (snd h) (peigneGauche t) Feuille



{-prop_hauteurPeigne xs = length xs == hauteur (peigneGauche xs)
prop_taillePeigne xs = length xs == taille (peigneGauche xs)
prop_hauteur'Peigne xs = length xs == hauteur' (peigneGauche xs)
prop_taille'Peigne xs = length xs == taille' (peigneGauche xs)
prop_mapArbrePeigne xs = peigneGauche xs == (mapArbre id id (peigneGauche xs))
-}

estComplet :: Arbre c a -> Bool
estComplet Feuille = True
estComplet (Noeud c v a1 a2) |hauteur' a1 == hauteur' a2 = estComplet a1 == estComplet a2
                             |otherwise = False

tamer :: (a->a->a)->a->a->a
tamer f x y = f x y

dimension' :: (a-> a -> a) -> Arbre coul val -> a
dimension' f (Noeud c v a1 a2) = f (dimension' f a1) (dimension' f a2)

estComplet' :: Arbre c a-> Bool
estComplet' Feuille = True
estComplet' (Noeud c v a1 a2) = (dimension' (\x y -> if x /= y then -1 else 0) (Noeud c v a1 a2)) == 0


hauteur'' :: Arbre coul val -> Int
hauteur'' Feuille = 0
hauteur'' a = 1 + (dimension' max a)

taille'' :: Arbre coul val -> Int
taille'' Feuille = 0
taille'' a = 1 + (dimension' (+) a)

{-Q10
Les peignes gauches complets sont quand l'abre est une Feuille ou quand les deux branches de l'arbre sont des Feuille
On peut utiliser QuickCheckcar on pourra observer qu'il s'arrétera au troisième test.
-}

complet :: Int -> [(c, a)] -> Arbre c a
complet 0 _ = Feuille
complet 1 ((c,v):_) = (Noeud c v Feuille Feuille)
complet 2 (x:y:z:xs) = (Noeud (fst(y)) (snd(y)) (complet (1) (x:xs)) (complet (1) (z:xs)))

complet h l = (Noeud c v (leftTree) (rightTree))
          where nb_de_noeuds = (length l) -1
                position_du_milieu = ((nb_de_noeuds ) `div` 2)
                c = fst (l !! (position_du_milieu) )
                v = snd (l !! (position_du_milieu) )
                leftTree = complet (h-1) (take (position_du_milieu) l)
                rightTree = complet (h-1) (drop (position_du_milieu+1) l)

{-complet2 :: Int -> [(c, a)] -> Arbre c a
complet2 0 _ = Feuille
complet2 1 ((c,v):_) = (Noeud c v Feuille Feuille)

complet2 x t = Noeud c v (complet2 (x-1) s1) (complet2 (x-1) s2)
               where (s1, ((c,v):s2)) = splitAt (length t `quot` 2) t
               -}
{-
(2^h) - 1 nombre de Noeud
2^ĥ

-}
mre :: a -> [a]
mre a = a : (mre a)

mre2 :: a -> [a]
mre2 a = iterate id a

q13 :: [Char] -> [((),Char)]
q13 []=[]
q13 (a:b) = ((),a) : (q13 b)

--q14
aplatit :: Arbre c a -> [(c,a)]
aplatit Feuille = []
aplatit (Noeud c a ab1 ab2)= (aplatit ab1) ++ [(c,a)] ++ (aplatit ab2)

element :: Eq a => a -> Arbre c a -> Bool
element _ Feuille = False
element a (Noeud c aa ab1 ab2) | a == aa = True
                             | otherwise = (element a ab1) || (element a ab2)
complet1 = complet 1 (take 15 (q13 "abcdefghijklmno"))

complet4 = complet 4 (q13 "abcdefghijklmno")
proptamer = map snd (aplatit complet4) == "abcdefghijklmno"

noeud :: (c -> String) -> (a -> String) -> (c,a) -> String
noeud fc fa (c,a) = (fc c)  ++ (fa a)

valarbre :: Arbre c a -> a
valarbre (Noeud c a ab1 ab2) = a

arcs :: Arbre c a -> [(a,a)]
arcs Feuille = []
arcs (Noeud c a Feuille Feuille) = []
arcs (Noeud c a arbre Feuille) = [(a, valarbre arbre)] ++ (arcs arbre)
arcs (Noeud c a Feuille arbre) = [(a, valarbre arbre)] ++ (arcs arbre)
arcs (Noeud c a arbre1 arbre2) = [(a, valarbre arbre1)] ++ [(a, valarbre arbre2)] ++ (arcs arbre1) ++ (arcs arbre2)

arc :: (a -> String) -> (a,a) -> String
arc fa (a1,a2) = (fa a1) ++ " -> " ++ (fa a2)

dotise :: String -> (c -> String) -> (a -> String) -> Arbre c a -> String
dotise s fc fa arbre = "/* Entête */\n" ++ "diagraph \"" ++ s ++"\" {\n" ++ "\t node [fontname =\"DejaVu-Sans\", shape=circle]\n"++"\n/*\tListe des nœuds*/\n" ++ unlines (map (noeud fc fa) (aplatit arbre)) ++ "\t/*Liste des arcs */\n"++ unlines (map (arc fa) (arcs arbre)) ++"}"




{-testArbre :: IO ()
testArbre = do
        writeFile "arbre.dot" (dotise "Test 1" (\_ -> "black") id complet4)
-}


elementR :: Ord a => a -> Arbre c a -> Bool
elementR _ Feuille = False
elementR e (Noeud _ a ab1 ab2) | e < a = elementR e ab1
                               | e > a = elementR e ab2
                               | otherwise = True

data Couleur = R|B deriving (Show,Eq)
type ArbreRN a = Arbre Couleur a

couleurToString :: Couleur -> String
couleurToString R = "red"
couleurToString B = "black"
              
equilibre :: ArbreRN a -> ArbreRN a
equilibre Feuille = Feuille
equilibre (Noeud _ z (Noeud R y (Noeud R x a b) c) d) = Noeud R y (Noeud B x a b) (Noeud B z c d)
equilibre (Noeud _ z (Noeud R x a (Noeud R y b c)) d) = Noeud R y (Noeud B x a b) (Noeud B z c d)
equilibre (Noeud _ x a (Noeud R z (Noeud R y b c) d)) = Noeud R y (Noeud B x a b) (Noeud B z c d)
equilibre (Noeud _ x a (Noeud R y b (Noeud R z c d))) = Noeud R y (Noeud B x a b) (Noeud B z c d)
equilibre abr = abr


racineB :: (Eq a,Ord a) => ArbreRN a -> ArbreRN a
racineB Feuille         = Feuille
racineB (Noeud _ x g d) = Noeud B x g d


insertionArbreRN :: (Eq a,Ord a)  => ArbreRN a -> a -> ArbreRN a
insertionArbreRN arbre valeur = racineB (ins valeur arbre)
  where ins v Feuille                              = Noeud R v Feuille Feuille
        ins v abr@(Noeud c r g d) | elementR v abr = abr
                                  | v < r          = equilibre (Noeud c r (ins v g) d)
                                  | otherwise      = equilibre (Noeud c r g (ins v d))


 {-                                 
 main = mapM_ ecrit arbres
     where ecrit a = do writeFile "arbre.dot" a
                        threadDelay 1000000
           arbres  = arbresDot "gcfxieqzrujlmdoywnbakhpvst"-}