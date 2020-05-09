module Envi where

import qualified Data.Map.Strict as M
import Carte
import System.Random
import Foreign.C.Types (CInt, CDouble (..) )

data Entite = Mob {iden :: Int , pvie :: Int, starting_time :: CDouble  }
    | Player {iden :: Int, pvie :: Int} | Treasure | Trap 
    deriving (Eq)

data Envi = Envi { contenu_envi :: M.Map Coord [ Entite ]} deriving (Show)


instance Show Entite where 
    show (Mob _ _ _) ="m"
    show (Player _ _) = "p"
    show Treasure  = "t"
    show Trap  = "T"


isPlayer :: Entite -> Bool 
isPlayer (Player _ _ ) = True
isPlayer _ = False


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






franchissable_env :: Coord -> Envi -> Bool
franchissable_env crd env= not $ M.member crd (contenu_envi env)

analyse :: Int -> (Coord, [Entite]) -> Maybe (Coord, Entite)
analyse _ (C _ _, []) = Nothing
analyse id (C x y, (Mob iden pvie starting_time):xs) =
    if id == iden
    then Just (C x y,(Mob iden pvie starting_time))
    else analyse id (C x y, xs)


trouve_id :: Int -> Envi -> Maybe (Coord, Entite)
trouve_id id (Envi contenu_envi) = 
    let liste = M.toAscList contenu_envi in
        aux id liste 
    where
        aux :: Int -> [(Coord, [Entite])] -> Maybe(Coord,Entite)
        aux _ [] = Nothing
        aux id ((C x y, entite):xs) =
            case analyse id (C x y, entite) of
                Just e -> Just e
                Nothing -> aux id xs


rmv_id ::  Int -> [Entite] -> [Entite]
rmv_id id ((Mob iden pvie starting_time):xs) = 
    if id == iden
    then xs 
    else ([(Mob iden pvie starting_time)]) <> rmv_id id xs 

rmv_coor :: Coord -> [(Coord,[Entite])] -> [(Coord, [Entite])]
rmv_coor (C x y) ((C x1 y1, entite):xs) =
    if x == x1 && y == y1
    then xs 
    else ([(C x1 y1, entite)]) <> rmv_coor (C x y) xs



rm_env_id :: Int -> Envi -> Envi
rm_env_id id (Envi contenu_envi) = 
    case trouve_id id (Envi contenu_envi) of
        Nothing -> (Envi contenu_envi)
        Just (C x y, (Mob iden pvie starting_time)) -> case (M.lookup (C x y) contenu_envi) of
                                                        Nothing -> error  "Should not occur"
                                                        Just ent -> let newEntite = rmv_id id ent in
                                                                        let newCase = (C x y, newEntite) in
                                                                            let liste_contenu =  M.toAscList contenu_envi in
                                                                                let newEnvi = rmv_coor (C x y) liste_contenu in 
                                                                                    (Envi (M.fromList(newEnvi <> [(newCase)]))) 

ajout_id :: Entite -> [Entite] -> [Entite]      --C'etait pas vraiment nécessaire..
ajout_id (Mob id pvie starting_time) xs =
    ([(Mob id pvie starting_time)]) <> xs


ajout :: (Coord,Entite) -> Envi -> Envi
ajout (C x y, (Mob id pvie starting_time)) (Envi contenu_envi) =
    case (M.lookup (C x y) contenu_envi) of
        Nothing -> error  "Should not occur"
        Just list_ent -> let newListEntite = ajout_id (Mob id pvie starting_time) list_ent in
                            let newCase = (C x y, newListEntite) in 
                                let liste_contenu =  M.toAscList contenu_envi in
                                    let newEnvi = rmv_coor (C x y) liste_contenu in
                                        (Envi (M.fromList(newEnvi <> [(newCase)])))


bouge_id :: Int -> Coord -> Envi -> Envi
bouge_id id (C x y) (Envi contenu_envi) =
    case trouve_id id (Envi contenu_envi) of
        Nothing -> error "should not occur"
        Just (C x1 y1, (Mob id pvie starting_time)) -> ajout (C x1 y1, (Mob id pvie starting_time)) (rm_env_id id (Envi contenu_envi))






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