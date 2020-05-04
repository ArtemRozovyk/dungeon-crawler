module Carte where 

import qualified Data.Map.Strict as M

data PDirection = NS | EO deriving Eq -- direction d’une porte
data StatutP = Ouverte | Fermee deriving Eq -- statut d’une porte
data Case = Normal -- une case vide
    | Porte PDirection StatutP -- une porte ouverte ou fermee
    | Mur -- infranchissable (sauf pour les fantomes ...)
    | Entree -- debut du niveau
    | Sortie -- fin du niveau
    deriving Eq

data Coord = C {cx :: Int , cy :: Int} deriving (Eq,Show)
data Carte = Carte { cartel :: Int , -- largeur
                     carteh :: Int , -- hauteur
                     carte_contenu :: (M.Map Coord Case)} -- cases de la carte

instance Ord Coord where
    (C x1 y1) <= (C x2 y2) = 
        if y1 <= y2
        then if y1 == y2 then x1 <= x2 else True
        else False

instance Show Case where
    show Normal = " "
    show Mur = "X"
    show Entree = "E"
    show Sortie = "S"
    show (Porte dir stat) | dir == NS && stat == Fermee = "-"  
    show (Porte dir stat) | dir == NS && stat == Ouverte = "\\"
    show (Porte dir stat) | dir == EO && stat == Fermee = "|"
    show (Porte dir stat) | dir == EO && stat == Ouverte = "/"

showCarte :: Carte -> String
showCarte (Carte largeur hauteur content) =
    let cases = M.toAscList content in
        aux cases 0 
    where
        aux :: [(Coord, Case)] -> Int -> String
        aux [] _ = ""
        aux ((C x y, caase):xs) lastY = 
            (if lastY == y then "" else "\n") <> (show caase) <> (aux xs y)

instance Show Carte where
    show = showCarte


readCase :: String -> [(Case, String)]
readCase [] = []
readCase (c:xs) | c ==' ' = [(Normal,xs)]
                | c =='X' = [(Mur,xs)]
                | c =='E' = [(Entree,xs)]
                | c =='S' = [(Sortie,xs)]
                | c =='-' = [(Porte NS Fermee,xs)]
                | c =='\\' = [(Porte NS Ouverte,xs)]
                | c == '|' = [(Porte EO Fermee,xs)]
                | c == '/' = [(Porte EO Ouverte,xs)]
                | c == '\n' = []
                | otherwise = []

instance Read Case where
    readsPrec _ = readCase

readAux :: String -> Int -> Int -> [(Coord, Case)] -> (Carte, String)
readAux [] haut larg cases = (Carte {carteh = haut, cartel = (cx (fst (last cases))) + 1, carte_contenu = M.fromDistinctDescList cases}, []) -- end string
readAux str line column acc = 
    let result = (reads :: ReadS Case) str in
        if result == []
        then if (head str) == '\n'
             then readAux (tail str) (line+1) 0 acc     -- saut à la ligne (str sous forme "\nES..")
             else (Carte {carteh = line, cartel = (cx (fst (last acc))) + 1, carte_contenu = M.fromDistinctDescList acc}, str)   -- le caractere lu invalide
        else let [(caase , reste)] = result in
            readAux reste line (column+1) ((C column line, caase):acc)    

instance Read Carte where
    readsPrec _  = (\ str -> [readAux str 0 0 []])

getCase :: Carte -> Int -> Int -> Maybe Case
getCase (Carte _ _ cases) x y = M.lookup (C x y) cases

isTraversable :: Carte -> Int -> Int -> Bool
isTraversable (Carte larg haut cases) x y = 
    case M.lookup (C x y) cases of
        Nothing -> False
        Just x -> x /= Mur

getEntreeCoord :: Carte -> Coord
getEntreeCoord (Carte larg haut cases) =
    let list_cases = M.toAscList cases in
        aux list_cases
    where
        aux [] = error "Should not occur"
        aux ((C x y, caase):xs) | caase == Entree = C x y
                                | otherwise = aux xs
{-
exit_acces_prop :: Carte -> Bool
exit_acces_prop carte@(Carte larg haut cases) =
    let coordEntry = getEntreeCoord carte in
        exists_path carte [coordEntry] []
    where
        exists_path _ [] _ = False
        exists_path carte ((C x y):coords) list =
             if (getCase x y) == Sortie 
             then True
             else if isTraversable (x-1) y 
                  then let 
                      -}
