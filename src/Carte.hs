module Carte where 
import System.IO
import qualified Data.Map.Strict as M
import SpriteMap (SpriteMap, SpriteId (..))
import qualified SpriteMap as SM
import Control.Monad (guard)
import Control.Concurrent (threadDelay)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (foldl',find)
import Foreign.C.Types (CInt (..) )
import SDL
import SDL.Time (time, delay)
import Linear (V4(..))
import TextureMap (TextureMap, TextureId (..))
import qualified TextureMap as TM
import Sprite (Sprite)
import qualified Sprite as S
import SpriteMap (SpriteMap, SpriteId (..))
import qualified SpriteMap as SM
import Keyboard (Keyboard)
import qualified Keyboard as K
import qualified Debug.Trace as T


data PDirection = NS | EO deriving Eq 
data StatutP = Ouverte | Fermee deriving Eq 
data Case = Empty
    | Porte PDirection StatutP 
    | Mur 
    | Entree 
    | Sortie 
    deriving Eq

data Coord = C {cx :: Int , cy :: Int} deriving (Eq,Show)
data Carte = Carte { cartel :: Int , 
                     carteh :: Int ,
                     carte_contenu :: (M.Map Coord Case)} 

instance Ord Coord where
    (C x1 y1) <= (C x2 y2) = 
        if y1 <= y2
        then if y1 == y2 then x1 <= x2 else True
        else False

instance Show Case where
    show Empty = " "
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
readCase (c:xs) | c ==' ' = [(Empty,xs)]
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

readCarte :: String -> Int -> Int -> [(Coord, Case)] -> (Carte, String)
readCarte [] haut larg cases = (Carte {carteh = haut+1, cartel = (cx (fst (head cases)))+1 , carte_contenu = M.fromDistinctDescList cases}, []) 
readCarte (hStr:tlStr) line column acc = 
        case (reads :: ReadS Case) (hStr:tlStr) of 
            []-> if hStr == '\n' then readCarte tlStr (line+1) 0 acc 
                                 else (Carte {carteh = line, cartel = (cx (fst (last acc))) + 1, carte_contenu = M.fromDistinctDescList acc}, (hStr:tlStr))
            [(caase , reste)] -> readCarte reste line (column+1) ((C column line, caase):acc)

instance Read Carte where
    readsPrec _  = (\ str -> [readCarte str 0 0 []])

getCase :: Carte -> Int -> Int -> Maybe Case
getCase (Carte _ _ cases) x y = M.lookup (C x y) cases

getCaseCoord :: Carte -> Int -> Int -> Maybe (Case,Coord)
getCaseCoord (Carte _ _ cases) x y = 
    let res =  M.lookup (C x y) cases in 
        case res of 
            Nothing -> Nothing 
            Just caz -> Just (caz,C x y)
            
isTraversable :: Carte -> Int -> Int -> Bool
isTraversable (Carte larg haut cases) x y = 
    case M.lookup (C x y) cases of
        Nothing -> False
        Just x -> x /= Mur && x /= (Porte NS Fermee) && x/= (Porte EO Fermee)

getEntreeCoord :: Carte -> Coord
getEntreeCoord (Carte larg haut cases) =
    case find (\(_,caze) -> caze == Entree) $ M.toList cases of 
        Nothing -> error "There is no Entrance"
        Just(crd, _)-> crd

getSortieCoord :: Carte -> Coord
getSortieCoord (Carte larg haut cases) =
    case find (\(_,caze) -> caze == Sortie) $ M.toList cases of 
        Nothing -> error "There is no Exit"
        Just(crd, _)-> crd

carteFromFile :: FilePath -> IO Carte
carteFromFile file = do 
    fd <- openFile file ReadMode
    str <- hGetContents fd
    return (read str :: Carte)

carteToFile :: Carte -> FilePath -> IO ()
carteToFile carte file = do 
    writeFile file (show carte)
    return ()

fetchSingleSprite :: Maybe String -> SpriteMap -> Int ->Int -> Sprite
fetchSingleSprite Nothing _ _ _ = error "Passed Nothing fetchSingleSprite"
fetchSingleSprite (Just name) smap x y = 
  (S.moveTo (SM.fetchSprite (SpriteId name) smap) (fromIntegral x) (fromIntegral y))

--fetches sprites that are already positionned correctly
fetchSpritesFromCarte :: Carte ->SpriteMap-> M.Map [Char] [Char] -> [Sprite]
fetchSpritesFromCarte carte smap mapTiles = 
  let contentList = M.toList(carte_contenu carte) in do
    (C x y,caze) <- contentList
    guard (M.member (show caze) mapTiles)
    return (fetchSingleSprite (M.lookup (show caze) mapTiles) smap (x*48) (y*48))

prop_inv_carte_saine :: Carte -> Bool 
prop_inv_carte_saine c =
    prop_carte1 c && prop_carte2 c  && prop_carte3 c && prop_carte4 c && prop_carte5 c && prop_carte6 c 

-- Invariants
prop_carte1:: Carte -> Bool  --Vérifie que toutes les cases soient bien comprisent entre la hauteur et la largeur
prop_carte1 (Carte larg haut cases) = 
    let list_cases = M.toAscList cases in 
        aux list_cases
    where
        aux [] = True 
        aux ((C x y, caase):xs) =
            if x > (larg-1) || y > (haut-1) 
            then False 
            else aux xs


aux2:: Carte -> Int -> Int -> Bool
aux2 (Carte _ _ cases) y 0 = case M.lookup (C y 0) cases of
                                         Nothing -> False
                                         Just x -> True
aux2 (Carte l h cases) y x = case M.lookup (C y x) cases of
                                         Nothing -> False
                                         Just a -> True &&  aux2 (Carte l h cases) y (x-1)

prop_carte2:: Carte -> Bool -- chaque case de la grille contient quelque chose 
prop_carte2 (Carte larg2 haut2 cases) =
    let (haut,larg)=(haut2-1,larg2-1) in
    aux1 (Carte larg haut cases) larg haut haut
    where
        aux1 :: Carte -> Int -> Int -> Int -> Bool 
        aux1 (Carte larg haut cases) lard hautd 0 = aux2 (Carte larg haut cases) 0 larg
        aux1 (Carte larg haut cases) lard hautd y = aux2 (Carte larg haut cases) y larg && aux1 (Carte larg haut cases) lard hautd (y-1) 


prop_carte3 :: Carte -> Bool  -- il y a une unique entree et unique sortie
prop_carte3 (Carte larg haut cases) =
    let list_cases = M.toAscList cases in 
        aux1 list_cases 0 0
    where
        aux1 :: [(Coord, Case)] -> Int -> Int -> Bool
        aux1 _ _ 2 = False
        aux1 _ 2 _ = False
        aux1 [] 1 1 = True
        aux1 [] _ _ = False 
        aux1 ((C x y, caase):xs) nbE nbS = 
            case caase of
                Entree -> aux1 xs (nbE + 1) nbS
                Sortie -> aux1 xs nbE (nbS + 1)
                otherwise -> aux1 xs nbE nbS

prop_carte4 :: Carte -> Bool -- Elle est entièrement entouré des murs 
prop_carte4 (Carte larg haut cases) =
    let list_cases = M.toAscList cases in 
        aux1 list_cases (larg-1) (haut-1)
    where
        aux1 :: [(Coord, Case)]  -> Int -> Int -> Bool
        aux1 [] _ _ = True
        aux1 ((C x y, caase):xs) larg haut =
            if (x == 0 || x == larg) && caase /= Mur
            then False
            else
                if(y==0 || y== haut) && caase /= Mur
                then False 
                else aux1 xs larg haut 

prop_carte5 :: Carte -> Bool --Chaque porte est encadre des murs
prop_carte5 (Carte larg haut cases) =
    let list_cases = M.toAscList cases in 
        aux1 (Carte larg haut cases) list_cases 
    where
        aux1 :: Carte -> [(Coord, Case)] -> Bool
        aux1 _ [] = True
        aux1 (Carte larg haut cases) ((C x y, caase):xs) = 
            case caase of
                Porte EO Ouverte -> if M.lookup (C x (y+1)) cases == Just Mur 
                                    then if M.lookup (C x (y-1)) cases == Just Mur 
                                        then aux1 (Carte larg haut cases) xs
                                        else False
                                    else False
                Porte EO Fermee -> if M.lookup (C x (y+1)) cases == Just Mur 
                                    then if M.lookup (C x (y-1)) cases == Just Mur 
                                        then aux1 (Carte larg haut cases) xs
                                        else False
                                    else False
                Porte NS Ouverte -> if M.lookup (C (x+1) y) cases == Just Mur 
                                    then if M.lookup (C (x-1) y) cases == Just Mur 
                                        then aux1 (Carte larg haut cases) xs
                                        else False
                                    else False
                Porte NS Fermee -> if M.lookup (C (x+1) y) cases == Just Mur 
                                    then if M.lookup (C (x-1) y) cases == Just Mur 
                                        then aux1 (Carte larg haut cases) xs
                                        else False
                                    else False
                otherwise -> aux1 (Carte larg haut cases) xs

getEntreeCase :: Carte -> (Coord,Case)
getEntreeCase (Carte larg haut cases) =
    let list_cases = M.toAscList cases in
        aux list_cases
    where
        aux [] = error "Should not occur"
        aux ((C x y, caase):xs) | caase == Entree = (C x y, caase)
                                | otherwise = aux xs

appartient :: (Coord, Case) -> [(Coord, Case)] -> Bool
appartient _ [] = True
appartient (C x1 y1, caase1) ((C x y, caase):xs) = if x == x1 && y == y1
                                                        then False 
                                                        else appartient (C x1 y1, caase1) xs

prop_carte6 :: Carte -> Bool --La sortie est accesible depuis l'entree 
prop_carte6 (Carte larg haut cases) =
    aux1 (Carte larg haut cases) [] [getEntreeCase (Carte larg haut cases)]
    where
        aux1 :: Carte -> [(Coord, Case)] -> [(Coord, Case)] -> Bool
        aux1 _ _ [] = False
        aux1 (Carte larg haut cases) v ((C x y, caase):xs) =
            case caase of 
                Sortie -> True
                _ -> case M.lookup (C x (y+1)) cases of
                                    Nothing -> error "Should not occur"
                                    Just e -> let (C x1 y1, e1) = (C x (y+1) ,e) in
                                                    if notElem (C x1 y1, e1) v && e1 /= Mur && notElem (C x1 y1, e1) xs
                                                    then let xss = (xs<>[(C x1 y1, e1)]) in
                                                         aux1 (Carte larg haut cases) (v<>[(C x y, caase)]) ([(C x y, caase)]<>xss) 
                                                    else case M.lookup (C x (y-1)) cases of
                                                            Nothing -> error "Should not occur"
                                                            Just e -> let (C x2 y2, e2) = (C x (y-1) ,e) in
                                                                if appartient (C x2 y2, e2) v && e2 /= Mur  && appartient (C x2 y2, e2) xs
                                                                then let xss = ([(C x2 y2, e2)]<>xs) in
                                                                    aux1 (Carte larg haut cases) (v<>[(C x y, caase)]) (xss<>[(C x y, caase)]) 
                                                                else case M.lookup (C (x+1) y) cases of
                                                                        Nothing -> error "Should not occur"
                                                                        Just e -> let (C x3 y3, e3) = (C (x+1) y ,e) in
                                                                            if appartient (C x3 y3, e3) v && e3 /= Mur  && appartient (C x3 y3, e3) xs
                                                                            then let xss = ([(C x3 y3, e3)]<>xs) in
                                                                                aux1 (Carte larg haut cases) (v<>[(C x y, caase)]) (xss<>[(C x y, caase)]) 
                                                                            else  case M.lookup (C (x-1) y) cases of
                                                                                        Nothing -> error "Should not occur"
                                                                                        Just e -> let (C x4 y4, e4) = (C (x-1) y ,e) in
                                                                                            if appartient (C x4 y4, e4) v && e4 /= Mur  && appartient (C x4 y4, e4) xs
                                                                                            then let xss = ([(C x4 y4, e4)]<>xs) in
                                                                                                aux1 (Carte larg haut cases) (v<>[(C x y, caase)]) (xss<>[(C x y, caase)]) 
                                                                                            else aux1 (Carte larg haut cases) ([(C x y, caase)]<>v) xs 

