import Parser
import Data.Maybe

import Data.List
main :: IO ()
main = undefined

type Nom = String

data Expression = Lam Nom Expression
                | App Expression Expression
                | Var Nom
                | Lit Litteral
                deriving (Show,Eq)

data Litteral = Entier Integer
              | Bool   Bool
              deriving (Show,Eq)

espacesP :: Parser ()
espacesP = (many (car ' ')) >>= \_ ->pure()
espacesP' :: Parser ()
espacesP' = do many (car ' ')
               pure()
lettre x = elem x (['a'..'z'])

nomP :: Parser Nom
nomP = (some (carQuand lettre))  >>= \nom -> espacesP >> pure(nom)

nomP' :: Parser Nom
nomP' = do nom <-some (carQuand lettre)
           espacesP
           pure(nom)

varP :: Parser Expression
varP = do nom <- nomP
          espacesP
          pure(Var nom)

applique :: [Expression] -> Expression
applique [] = error""
applique [x] = x
applique (e1:e2:t)= applique ((App e1 e2):t)

applique' :: [Expression] -> Expression
applique' =foldl1 App

exprP :: Parser Expression
exprP = (varP <|> nombreP <|>lambdaP <|> exprParentheseeP <|>booleenP)

exprsP :: Parser Expression
exprsP = do expr <- some exprP
            pure(applique' expr)

lambdaP :: Parser Expression
lambdaP = do chaine "\\" <|> chaine "λ"
             espacesP
             x <-nomP
             chaine "-> "
             expr <-exprsP
             pure(Lam x expr)

exprParentheseeP :: Parser Expression
exprParentheseeP = do car '('
                      expr <- exprsP
                      car ')'
                      espacesP
                      pure(expr)

chiffre x = elem x ['1'..'9']

nombreP :: Parser Expression
nombreP = do nb <-some (carQuand chiffre)
             espacesP
             pure(Lit (Entier (read nb::Integer)))



booleenP :: Parser Expression
booleenP = do b <-(chaine "True" <|> chaine "False")
              espacesP
              pure(Lit (Bool (read b::Bool)))

expressionP :: Parser Expression
expressionP = do espacesP
                 ep <- exprsP
                 espacesP
                 pure(ep)

ras :: String -> Expression
ras s | (runParser expressionP s) == Nothing = error "Erreur d’analyse syntaxique"
      | otherwise = fst (fromJust (runParser expressionP s))

data ValeurA = VLitteralA Litteral
             | VFonctionA (ValeurA -> ValeurA)


instance Show ValeurA where
   show (VFonctionA _)          = "λ"
   show (VLitteralA (Entier n)) = show n
   show (VLitteralA (Bool n)) = show n

type Environnement a = [(Nom, a)]

interpreteA :: Environnement ValeurA -> Expression -> ValeurA
interpreteA _ (Lit a) = VLitteralA a
interpreteA env (Var n) = fromJust(lookup n env)
interpreteA env (Lam n vr) = VFonctionA (\v -> interpreteA ((n,v):env) vr)
interpreteA env (App e1 e2) =f (interpreteA env e2)
                             where VFonctionA f = interpreteA env e1  


negA :: ValeurA
negA = VFonctionA (\(VLitteralA (Entier x))-> (VLitteralA (Entier (-x))))

--la fonction prend 1 seul arg
addA :: ValeurA
addA = VFonctionA (\(VLitteralA (Entier x))-> VFonctionA(\(VLitteralA (Entier y))  -> (VLitteralA (Entier (x+y)))))

envA :: Environnement ValeurA
envA = [ ("neg",   negA)
       , ("add",   releveBinOpEntierA (+))
       , ("soust", releveBinOpEntierA (-))
       , ("mult",  releveBinOpEntierA (*))
       , ("quot",  releveBinOpEntierA quot) ]


releveBinOpEntierA :: (Integer -> Integer -> Integer) -> ValeurA
releveBinOpEntierA op = VFonctionA (\(VLitteralA (Entier x))-> VFonctionA(\(VLitteralA (Entier y))  -> (VLitteralA (Entier (op x y)))))

ifthenelseA :: ValeurA
ifthenelseA = VFonctionA (\(VLitteralA (Bool b)) ->(VFonctionA (\(VLitteralA x)-> VFonctionA(\(VLitteralA y)  -> if b == True 
                                                                                                                 then(VLitteralA x)
                                                                                                                else (VLitteralA y)))))
                                                                                                                
main :: IO()
main = do putStr "minilang> "
          cmd <- getLine
          if (length cmd == 0) then
            exitSuccess
          else
            print (interpreteA envA (ras cmd))
          main

data Either a b = Left a
                | Right b

data ValeurB = VLitteralB Litteral
             | VFonctionB (ValeurB -> ErrValB)

type MsgErreur = String
type ErrValB   = Either MsgErreur ValeurB

instance Show ValeurB where
    show (VFonctionB _)          = "λ"
    show (VLitteralB (Entier n)) = show n
    show (VLitteralB (Bool n))   = show n

-- question 22
interpreteB :: Environnement ValeurB -> Expression -> ErrValB
interpreteB _   (Lit x)   = Right (VLitteralB x)
interpreteB env (Var x)   = case lookup x env of
                                    Nothing -> Left ("Erreur d'interprétation : la variable " ++ x ++ " n'est pas défini")
                                    Just v  -> Right v
interpreteB env (Lam x y) = Right (VFonctionB (\v -> interpreteB ((x, v):env) y))
interpreteB env (App x y) = case interpreteB env x of
                                    e@(Left _)           -> e
                                    Right (VFonctionB f) -> case (interpreteB env y) of
                                                                    e@(Left _) -> e
                                                                    Right v    -> f v
                                    Right e              -> Left ("Erreur d'interprétation : fonction attendu, mais " ++ (show e) ++ " trouvé")


-- question 23
addB :: ValeurB
addB = VFonctionB f
       where f (VLitteralB (Entier x)) = Right (VFonctionB g)
                    where g (VLitteralB (Entier y)) = Right (VLitteralB (Entier (x + y)))
                          g e                       = Left ("Erreur d'interprétation (addB) : nombre entier attendu en deuxième paramètre, mais " ++ (show e) ++ " trouvé")
             f e = Left ("Erreur d'interprétation (addB) : nombre entier attendu en premier paramètre, mais " ++ (show e) ++ " trouvé")


-- question 24
quotB :: ValeurB
quotB = VFonctionB f
       where f (VLitteralB (Entier x)) = Right (VFonctionB g)
                    where g (VLitteralB (Entier 0)) = Left ("Erreur d'interprétation (quotB) : division par 0 impossible")
                          g (VLitteralB (Entier y)) = Right (VLitteralB (Entier (x `quot` y)))
                          g e                       = Left ("Erreur d'interprétation (quotB) : nombre entier attendu en deuxième paramètre, mais " ++ (show e) ++ " trouvé")
             f e = Left ("Erreur d'interprétation (quotB) : nombre entier attendu en premier paramètre, mais " ++ (show e) ++ " trouvé")




-- interprète traçant


data ValeurC = VLitteralC Litteral
             | VFonctionC (ValeurC -> OutValC)

type Trace   = String
type OutValC = (Trace, ValeurC)

-- question 25
instance Show ValeurC where
    show (VFonctionC _)          = "λ"
    show (VLitteralC (Entier n)) = show n
    show (VLitteralC (Bool n))   = show n


interpreteC :: Environnement ValeurC -> Expression -> OutValC
interpreteC _   (Lit x)   = ("", VLitteralC x)
interpreteC env (Var x)   = ("", fromJust (lookup x env))
interpreteC env (Lam x y) = ("", VFonctionC (\v -> interpreteC ((x, v):env) y))
interpreteC env (App x y) = case interpreteC env x of
                                    (t, (VFonctionC f)) -> ((t++"."++(fst application)), snd application)
                                                            where application = f (snd (interpreteC env y))
                                    e                   -> error ("Erreur d'interprétation : fonction attendu, mais " ++ (show e) ++ " trouvé")

pingC :: ValeurC
pingC = VFonctionC (\x -> ("p", x))




data ValeurM m = VLitteralM Litteral
               | VFonctionM (ValeurM m -> m (ValeurM m))

instance Show (ValeurM m) where
    show (VFonctionM _)          = "λ"
    show (VLitteralM (Entier n)) = show n
    show (VLitteralM (Bool n))   = show n

