module State where

import Carte 
import Envi
import Model
import qualified Data.Map.Strict as M
import Control.Monad (unless,foldM,mapM_)
import System.Random
import Sprite (Sprite)
import qualified Sprite as S
import Keyboard (Keyboard)
import Foreign.C.Types (CDouble (..) )

import SDL.Time (time)

import SpriteMap (SpriteMap, SpriteId (..))
import qualified SpriteMap as SM

data Etat = Perdu
    | Gagne
    | Tour {
    num_tour :: Int,
    carte_tour :: Carte,
    envi_tour :: Envi,
    gen_tour :: StdGen,
    obj_tour :: (M.Map Int Entite),
    log :: String}


instance Show Etat where
    show e = show $carte_tour e



prop_state_inv :: Etat -> Bool 
prop_state_inv e@(Tour nt ct env gt ot lgt) = 
    prop_state_inv1(e) && prop_state_inv2(e) && prop_state_inv3(e) && prop_state_inv4(e) && prop_inv_carte_saine(ct) && prop_envi_inv(env)
    --numTour est un entier poitif
    --Chaque coordonnée d'entite correspond à une case traversable
    --Les entités sont bien dans les limites de la carte 
    --Les mobs ne sont pas l'un sur l'autre (ni sur le joueur) pas de plusieurs
    --  entités dans une même case (parce que.) 
    --  

prop_state_inv1 :: Etat -> Bool  --numTour est un entier naturel
prop_state_inv1 etat@(Tour nt ct env gt ot lgt) =
    nt>=0

prop_state_inv2 :: Etat -> Bool --Chaque coordonnée d'entite correspond à une case traversable
prop_state_inv2 etat@(Tour nt ct env gt ot lgt) =
    let liste_ent = M.toAscList(contenu_envi env) in 
        aux liste_ent ct
        where
        aux :: [(Coord,[Entite])] -> Carte -> Bool 
        aux [] _ = True 
        aux ((C x y,entites):xs) c = 
            if entites /= [] 
            then isTraversable c x y && aux xs c
            else aux xs c

prop_state_inv3 :: Etat -> Bool   --Les entités sont bien dans les limites de la carte 
prop_state_inv3 etat@(Tour nt ct env gt ot lgt) =
     let liste_ent = M.toAscList(contenu_envi env) in 
        aux liste_ent ct
        where
        aux :: [(Coord,[Entite])] -> Carte -> Bool 
        aux [] _ = True
        aux ((C x y,entites):xs) c@(Carte l h carte_contenu) = 
            if entites /= [] 
            then x<l && y<h && aux xs c
            else aux xs c

prop_state_inv4 :: Etat -> Bool --Les mobs ne sont pas l'un sur l'autre (ni sur le joueur) pas de plusieurs entites dans une case
prop_state_inv4 etat@(Tour nt ct env gt ot lgt) =
     let liste_ent = M.toAscList(contenu_envi env) in 
        aux liste_ent ct
        where
        aux :: [(Coord,[Entite])] -> Carte -> Bool 
        aux [] _ = True 
        aux ((C x y,entites):xs) c = 
            if entites /= [] 
            then isSeul entites && aux xs c
            else aux xs c

prop_pre_add_entity_state :: Etat -> Coord -> Entite-> Bool 
prop_pre_add_entity_state e crd m =
    prop_pre_add_entity_state1 e crd m  && prop_pre_add_entity_state2 e crd m && prop_pre_add_entity_state3 e crd m && prop_pre_add_entity_state4 e crd m


prop_pre_add_entity_state1 :: Etat -> Coord -> Entite-> Bool -- Les coordonnées sont comprises dans les limites de la carte
prop_pre_add_entity_state1 e@(Tour nt ct@(Carte l h carte_contenu) env gt ot lgt) crd@(C x y) m = 
    x>=1 && x<l && y>=1 && y<h

prop_pre_add_entity_state2 :: Etat -> Coord -> Entite-> Bool -- Vérifie qu'il n'y a pas d'entités présente sur la case
prop_pre_add_entity_state2 e@(Tour nt ct env gt ot lgt) crd@(C x y) m = 
    case M.lookup (C x y) (contenu_envi env) of 
        Nothing -> True
        Just _ -> False

nb_mobs :: [(Coord,[Entite])] -> Int 
nb_mobs [] = 0
nb_mobs ((C x y,entites):xs) = 
    if entites /= [] 
    then 1 + nb_mobs xs 
    else nb_mobs xs  

nb_cases_libres :: Carte -> Int 
nb_cases_libres c@(Carte l h carte_contenu) =
    let liste_cases = M.toAscList carte_contenu in 
        aux liste_cases c
        where 
            aux :: [(Coord, Case)] -> Carte -> Int 
            aux [] _ = 0
            aux ((C x y, caase):xs) c = 
                if isTraversable c x y
                then 1 + aux xs c 
                else aux xs c

prop_pre_add_entity_state3 :: Etat -> Coord -> Entite -> Bool -- le nombre de mobs sur la carte ne peut pas depasser la moitié des cases vides de la carte(trop peuplé sinon)
prop_pre_add_entity_state3 e@(Tour nt ct env gt ot lgt) crd@(C x y) m = 
    let nb_mobs_p = (nb_mobs (M.toAscList(contenu_envi env))) + 1 in
        let nb_cases = nb_cases_libres ct in
            nb_mobs_p<(nb_cases `div` 2) 

prop_pre_add_entity_state4 :: Etat -> Coord -> Entite -> Bool -- on ne peut pas ajouter plus d'un joueur 
prop_pre_add_entity_state4 e@(Tour nt ct env gt ot lgt) crd@(C x y) m = 
    if not (isPlayer(m))
    then True 
    else let liste_ent = M.toAscList(contenu_envi env) in 
        aux liste_ent
        where
        aux :: [(Coord,[Entite])] -> Bool 
        aux [] = True 
        aux ((C x y,entites):xs) = 
            if isPlayer(head entites)
            then False
            else aux xs 

prop_post_add_entity_state :: Etat -> Coord -> Entite -> Bool --l'entite a bien été rajouté
prop_post_add_entity_state e@(Tour nt ct env gt ot lgt) crd@(C x y) ent@(Mob id pv st) =
    case M.lookup (C x y) (contenu_envi env) of
        Nothing -> error "should not happend"
        Just a -> let m@(Mob id1 pv1 st1) = (head a) in
            (isMob(ent) && isMob(head a)) || (isPlayer(ent) && isPlayer(head a))


    

add_entity_state :: Etat -> Coord -> Entite -> Etat 
add_entity_state s c e =
    let id = (foldl max 0 (M.keys $ obj_tour s ))+1 in
    let ent = if(not $ isObject $ e) then e{iden=id} else e in
    let ot = M.insert id ent (obj_tour s) in 
    if M.member c (contenu_envi (envi_tour s)) --there are already entities 
    then s { envi_tour= Envi $ M.adjust (++[ent]) c (contenu_envi (envi_tour s)), obj_tour=ot}
    else s { envi_tour= Envi $ M.insert c [ent] (contenu_envi (envi_tour s)), obj_tour=ot }


empty_state :: Carte-> StdGen -> Etat 
empty_state carte gen = 
    Tour 0 carte (Envi M.empty) gen M.empty ""

change_etat :: Etat -> Etat
change_etat etat@(Tour nt ct env gt ot lgt) =
    let (crdPlayer,player) = getPlayer env in 
        let crdSortie = getSortieCoord ct in
            if (not $ trapIsPresent env) then Perdu else 
            if (hasTreasure $ head  player) && crdPlayer == crdSortie then Gagne 
            else Tour nt ct env gt ot lgt

makeNEntities :: Int -> StdGen -> CDouble -> [Entite] 
makeNEntities n gen moment = 
    let timesl= getNRandom [0..11] n gen False in 
        map (\t -> Mob 0 100 (moment+t) ) timesl 


init_state :: Carte -> Int -> CDouble-> StdGen -> Etat -- (Envi, M.Map Int Entite )
init_state carte n moment gen =
    let (gen',g) = split gen in
    let entites = (Trap):(Treasure):(makeNEntities n g moment) in
    let emptyCases =  filter (\(_,c)-> c == Empty ) (M.toList $ carte_contenu carte) in 
    let places = getNRandom  emptyCases (length entites) gen' True in
    let mbs= foldl (\e (c,m) ->  add_entity_state e c m) (empty_state carte gen') (zip (map (\(c,_)-> c) places) entites) in
        let player = [Player 42 100 False] in
        mbs {envi_tour = Envi $ M.insert (getEntreeCoord carte) player (contenu_envi (envi_tour mbs)),
        obj_tour = M.insert 42 (head player) (obj_tour mbs)} 

initGameState :: Carte-> StdGen ->CDouble-> Etat
initGameState carte gen moment = do 
     init_state carte 2 moment gen  

fetchSpritesFromEnv :: Etat ->SpriteMap-> M.Map [Char] [Char] -> [Sprite]
fetchSpritesFromEnv state smap mapTiles = 
    let env = M.toList (contenu_envi $ envi_tour state) in do 
        (C x y,ents) <- env  --TODO multiple entities in single case
        let ent = head ents 
        return (fetchSingleSprite (M.lookup (show ent) mapTiles) smap (x*48) (y*48))


etat_tour ::Etat -> Keyboard ->CDouble ->StdGen-> Etat
--call tour de toutes les entités obj_tour. win or loss? 
etat_tour etat@(Tour nt ct et gt ot lgt) kbd moment gen= 
    let (g1,g2) = split gen in
    let modele' = Model ct et g1 "" kbd in
    let modele = stepPlayer modele' kbd in
    let modeleF= stepMobs modele moment in
        Tour nt (carte modeleF) (envi modeleF) g2 ot ((log_m modeleF)++lgt)
etat_tour e _ _ _  = e

    --let modeleF =stepPlayer modele' in 

     

