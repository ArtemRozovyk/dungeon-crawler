module Envi where

import qualified Data.Map.Strict as M
import Carte
import System.Random
import Foreign.C.Types (CInt, CDouble (..) )
import Data.Maybe
data Entite = Mob {iden :: Int , pvie :: Int, starting_time :: CDouble  }
    | Player {iden :: Int, pvie :: Int, hasTreasure ::Bool} | Treasure | Trap 
    deriving (Eq)

data Envi = Envi { contenu_envi :: M.Map Coord [ Entite ]} deriving (Show)


instance Show Entite where 
    show (Mob _ _ _) ="m"
    show (Player _ _ _) = "p"
    show Treasure  = "t"
    show Trap  = "T"


isPlayer :: Entite -> Bool 
isPlayer (Player _ _ _ ) = True
isPlayer _ = False

isSeul :: [Entite] -> Bool
isSeul (ent:xs) = if xs == []
                then True 
                else False


prop_envi_inv :: Envi -> Bool       -- Vérifie que l'environnement est saint
prop_envi_inv env =
    prop_envi_inv1(env) && prop_envi_inv2(env) && prop_envi_inv3(env)

prop_envi_inv1 :: Envi -> Bool      -- Vérifie qu'il n'y ait pas 2 entités au meêm endroit 
prop_envi_inv1 env =
     let liste_ent = M.toAscList(contenu_envi env) in 
        aux liste_ent 
        where
        aux :: [(Coord,[Entite])] -> Bool 
        aux []  = True 
        aux ((C x y,entites):xs)  = 
            if entites /= [] 
            then isSeul entites && aux xs 
            else aux xs 

prop_envi_inv2 :: Envi -> Bool          --Vérifie qu'il n'y ait pas plus d'un seul joueur
prop_envi_inv2 env =
     let liste_ent = M.toAscList(contenu_envi env) in 
        aux liste_ent False 
        where 
            aux :: [(Coord,[Entite])] -> Bool -> Bool 
            aux [] _ = True 
            aux ((C _ _,entites):xs) True =
                if isPlayer(head entites)
                then False 
                else aux xs True 
            aux ((C _ _,entites):xs) False =
                if isPlayer(head entites)
                then aux xs True 
                else aux xs False 

prop_envi_inv3 :: Envi -> Bool      -- Vérifie que toutes les coordonnés > 0 
prop_envi_inv3 env =
     let liste_ent = M.toAscList(contenu_envi env) in 
        aux liste_ent 
        where
        aux :: [(Coord,[Entite])] -> Bool 
        aux []  = True 
        aux ((C x y,entites):xs)  = 
            if x > 0 && y > 0 
            then aux xs 
            else False  

isMob :: Entite -> Bool 
isMob (Mob _ _ _) = True
isMob _ = False

isObject :: Entite -> Bool 
isObject Treasure = True
isObject Trap = True
isObject _ = False

isTrap :: Entite -> Bool 
isTrap Trap = True
isTrap _ = False

isTreasure :: Entite -> Bool 
isTreasure Treasure = True
isTreasure _ = False

trouve_env_Cord  :: Envi -> Coord -> Maybe Entite 
trouve_env_Cord e crd  =  (M.lookup crd $ contenu_envi e)>>=(\x-> Just $ head x )  

franchissable_env :: Coord -> Envi -> Bool -> Bool --B a True si l'entitée appelante peux marcher sur un piege
franchissable_env (C x y) e b =
    case M.lookup (C x y) (contenu_envi e) of
        Nothing -> True
        Just list_ent -> aux list_ent
        where 
            aux :: [Entite] -> Bool 
            aux [] = True 
            aux (entite:xs) =
                if isMob(entite) || isPlayer(entite) || isTreasure(entite) || (not $ b && isTrap(entite)) 
                then False
                else aux xs

pre_rmv_envi :: Coord -> Envi -> Bool   --Vérifie qu' il y a bien une entité dans l'envorionnement a ces coordonnées
pre_rmv_envi (C x y) env@(Envi cont) =
    case M.lookup (C x y) cont of
        Nothing -> False 
        Just a -> a /= []

rmv_coor_envi :: Coord -> Envi -> Envi 
rmv_coor_envi crd env@(Envi cont) =
    Envi $ M.delete crd cont

post_rmv_envi :: Coord -> Envi -> Bool      --Vérifie qu'il n'y ait plus d'entités à ces coordonnées
post_rmv_envi (C x y) env@(Envi cont) =
    case M.lookup (C x y) cont of
        Nothing -> True
        Just a -> a == []

getRandom :: (Eq a) => [a] -> StdGen -> (a,StdGen)
getRandom lst gen = let res = randomR (0, (length lst)-1) gen in 
     ((lst !! fst res),snd res)

getNRandomAux :: (Eq a) => [a] -> Int-> StdGen-> [a] -> Bool -> [a] 
getNRandomAux _ 0 gen acc unique= acc
getNRandomAux l n gen acc unique= 
    let (v,g) = getRandom l gen in
        if (unique && (v `elem` acc)) 
            then getNRandomAux l n g acc unique
            else getNRandomAux l (n-1) g (v:acc) unique

getNRandom ::(Eq a) => [a] -> Int -> StdGen-> Bool-> [a] 
getNRandom l n gen unique = if n<= length l then 
    getNRandomAux l n gen [] unique
    else fail "Out of range"
    
getPlayer :: Envi -> (Coord, [Entite])
getPlayer e = head (M.toList (M.filter (\x -> isPlayer $ head x) (contenu_envi e))) 

post_getPlayer :: (Coord, [Entite]) -> Bool         --Vérifie qu'on ait bien retourné un Joueur
post_getPlayer (C x y,entites) =
    isPlayer(head entites)


trapIsPresent :: Envi -> Bool 
trapIsPresent e = M.filter (\x -> isTrap $ head x) (contenu_envi e) /= M.empty 

pre_ajout_env :: (Coord,Entite) -> Envi -> Bool         --Vérifie qu'il n'y a pas déjà un mob a ces coordonnés
pre_ajout_env (crd,ent) env@(Envi cnt) =                
    case M.lookup crd (contenu_envi env) of
        Nothing -> True 
        Just a -> (not $ isMob(head a)) && (not $ isPlayer(head a))

ajout_env :: (Coord,Entite) -> Envi -> Envi 
ajout_env (crd,ent) env@(Envi cnt) = 
    env{contenu_envi=M.insert crd [ent] cnt}

