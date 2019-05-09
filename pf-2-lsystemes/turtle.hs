
import Graphics.Gloss

type Symbole  = Char
type Mot      = [Symbole]
type Axiome   = Mot
type Regles   = Symbole -> Mot
type LSysteme = [Mot]

motSuivant :: Regles -> Mot -> Mot
motSuivant r [] = []
motSuivant r (h:t) = r h ++ (motSuivant r t)


motSuivant' :: Regles -> Mot -> Mot
motSuivant' r mot = (concatMap r mot)


motSuivant'' :: Regles -> Mot -> Mot
motSuivant'' r mot = concat [r elemL | elemL <- mot]



bella :: Regles


bella 'F' = "F-F++F-F"
bella '+' = "+"
bella '-' = "-"

lsysteme :: Axiome -> Regles -> LSysteme
lsysteme a r = iterate (motSuivant r) a

--Q4
type EtatTortue = (Point, Float)

type Config = (EtatTortue -- État initial de la tortue
              ,Float      -- Longueur initiale d’un pas
              ,Float      -- Facteur d’échelle
              ,Float      -- Angle pour les rotations de la tortue
              ,[Symbole]) -- Liste des symboles compris par la tortue

etatInitial :: Config -> EtatTortue
etatInitial (e,_,_,_,_) = e

longueurPas :: Config -> Float
longueurPas (_,l,_,_,_) = l


facteurEchelle :: Config -> Float
facteurEchelle (_,_,f,_,_) = f

angle :: Config -> Float
angle (_,_,_,a,_) =a

symbolesTortue :: Config -> [Symbole]
symbolesTortue (_,_,_,_,s) = s

avance :: Config -> EtatTortue -> EtatTortue
avance c ((x,y),cap) = ((x + (longueurPas c)*cos(cap),y + (longueurPas c)*sin(cap)),cap)

tourneAGauche :: Config -> EtatTortue -> EtatTortue
tourneAGauche c (p,cap)= (p,cap + (angle c))


tourneADroite :: Config -> EtatTortue -> EtatTortue
tourneADroite c (p,cap)= (p,cap - (angle c))

member :: Eq a => a -> [a] -> Bool
member a [] = False
member a (x:xs) | a == x = True
                | otherwise = member a (xs)

filtreSymbolesTortue :: Config -> Mot -> Mot
filtreSymbolesTortue c [] = []
filtreSymbolesTortue c (l:ls) | member l (symbolesTortue c) = l: filtreSymbolesTortue c ls
                              | otherwise =  filtreSymbolesTortue c ls

type EtatDessin = (EtatTortue, Path)

interpreteSymbole :: Config -> EtatDessin -> Symbole -> EtatDessin
interpreteSymbole conf (eTi,p) s =(etatsuivant,(fst etatsuivant):p)
                                  where etatsuivant| s == 'F' = avance conf eTi
                                                   | s == '+' = tourneAGauche conf eTi
                                                   | s == '-' = tourneADroite conf eTi
                                                   | otherwise = error "mauvais symbole"

{-Q9
 Ici l'ajout est en début de liste.
 L'ajout en début de liste est moins coûteux que celui en fin de liste sur un chemin important,
 pour l'ajout en fin de liste il faudra parcourir toute la liste avant de pouvoir concaténer.
-}



interpreteMot :: Config -> Mot -> Picture
interpreteMot c m = line (snd (foldl (interpreteSymbole c) iE mF))
    where iP = fst (etatInitial c)
          iE = (etatInitial c, [iP])
          mF = filtreSymbolesTortue c m





lsystemeAnime :: LSysteme -> Config -> Float -> Picture
lsystemeAnime ls c t = interpreteMot conf (ls !! enieme)
  where enieme = round t `mod` 8
        conf = case c of
          (e, p, fE, a, s) -> (e, p * (fE ^ enieme), fE, a, s)






vonKoch1 :: LSysteme
vonKoch1 = lsysteme "F" regles
    where regles 'F' = "F-F++F-F"
          regles  s  = [s]

vonKoch2 :: LSysteme
vonKoch2 = lsysteme "F++F++F++" regles
    where regles 'F' = "F-F++F-F"
          regles  s  = [s]

hilbert :: LSysteme
hilbert = lsysteme "X" regles
    where regles 'X' = "+YF-XFX-FY+"
          regles 'Y' = "-XF+YFY+FX-"
          regles  s  = [s]

dragon :: LSysteme
dragon = lsysteme "FX" regles
    where regles 'X' = "X+YF+"
          regles 'Y' = "-FX-Y"
          regles  s  = [s]

vonKoch1Anime :: Float -> Picture
vonKoch1Anime = lsystemeAnime vonKoch1 (((-400, 0), 0), 800, 1/3, pi/3, "F+-")

vonKoch2Anime :: Float -> Picture
vonKoch2Anime = lsystemeAnime vonKoch2 (((-400, -250), 0), 800, 1/3, pi/3, "F+-")

hilbertAnime :: Float -> Picture
hilbertAnime = lsystemeAnime hilbert (((-400, -400), 0), 800, 1/2, pi/2, "F+-")

dragonAnime :: Float -> Picture
dragonAnime = lsystemeAnime dragon (((0, 0), 0), 50, 1, pi/2, "F+-")


dessin = interpreteMot (((-150,0),0),100,1,pi/3,"F+-") "F+F--F+F"

main = animate (InWindow "L-système" (1000, 1000) (0, 0)) white vonKoch1Anime
