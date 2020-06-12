module Model where
import qualified Data.Map.Strict as M
import qualified Data.List as L
import qualified Keyboard as K
import Data.Set as S
import Foreign.C.Types (CDouble (..) )

import SDL
import Keyboard (Keyboard)
import System.Random 
import Keyboard 
import Carte
import Envi
import Data.Maybe
data Ordre = N | S | E | O | U | R deriving Show

data Modele = Model {
  carte :: Carte , 
  envi :: Envi , 
  gene :: StdGen , 
  log_m :: String , 
  keyboard :: Keyboard }



instance Show Modele where 
  show m = (show $envi m ) <> (show $ carte m) 

prop_modele_inv :: Modele -> Bool
prop_modele_inv m@(Model ct env g lgt kbd) =
  prop_modele_inv1 m && prop_modele_inv2 m  && prop_inv_carte_saine ct && prop_envi_inv env && prop_modele_inv4 m


prop_modele_inv1 :: Modele -> Bool    --Vérifie que les entités sont bien sur une case traversable
prop_modele_inv1 m@(Model ct env g lgt kbd) =
    let liste_ent = M.toAscList(contenu_envi env) in 
        aux liste_ent ct
        where
        aux :: [(Coord,[Entite])] -> Carte -> Bool 
        aux [] _ = True 
        aux ((C x y,entites):xs) c = 
            if entites /= [] 
            then isTraversable c x y && aux xs c
            else aux xs c

prop_modele_inv2 :: Modele -> Bool    --Vérifie que les entités sont bien dans les limites de la carte
prop_modele_inv2 m@(Model ct env g lgt kbd) =
     let liste_ent = M.toAscList(contenu_envi env) in 
        aux liste_ent ct
        where
        aux :: [(Coord,[Entite])] -> Carte -> Bool 
        aux [] _ = True
        aux ((C x y,entites):xs) c@(Carte l h carte_contenu) = 
            if entites /= [] 
            then x<l && y<h && aux xs c
            else aux xs c


getCoordTresor :: Modele -> Coord 
getCoordTresor m@(Model ct env g lgt kbd) =
  let liste_env = M.toList(contenu_envi env) in 
    aux liste_env 
    where
      aux :: [(Coord,[Entite])] -> Coord 
      aux [] = error"Le trésor n'est pas sur la map"
      aux ((C x y,entites):xs) =
        if isTreasure(head entites)
        then C x y
        else aux xs


prop_modele_inv4 :: Modele -> Bool         --Vérifie si le trésor est accesible depuis l'entrée.
prop_modele_inv4 m@(Model ct@(Carte larg haut cases) env g lgt kbd) =
  let (C x6 y6) = getCoordTresor m in
    aux1 (Carte larg haut cases) [] [getEntreeCase (Carte larg haut cases)] (C x6 y6)
    where
        aux1 :: Carte -> [(Coord, Case)] -> [(Coord, Case)] -> Coord -> Bool
        aux1 _ _ [] _ = False
        aux1 (Carte larg haut cases) v ((C x y, caase):xs) (C x6 y6) =
            if (x==x6) && (y==y6)
            then True
            else case M.lookup (C x (y+1)) cases of
                                    Nothing -> error "Should not occur"
                                    Just e -> let (C x1 y1, e1) = (C x (y+1) ,e) in
                                                    if notElem (C x1 y1, e1) v && e1 /= Mur && notElem (C x1 y1, e1) xs
                                                    then let xss = (xs<>[(C x1 y1, e1)]) in
                                                         aux1 (Carte larg haut cases) (v<>[(C x y, caase)]) ([(C x y, caase)]<>xss) (C x6 y6) 
                                                    else case M.lookup (C x (y-1)) cases of
                                                            Nothing -> error "Should not occur"
                                                            Just e -> let (C x2 y2, e2) = (C x (y-1) ,e) in
                                                                if appartient (C x2 y2, e2) v && e2 /= Mur  && appartient (C x2 y2, e2) xs
                                                                then let xss = ([(C x2 y2, e2)]<>xs) in
                                                                    aux1 (Carte larg haut cases) (v<>[(C x y, caase)]) (xss<>[(C x y, caase)]) (C x6 y6)
                                                                else case M.lookup (C (x+1) y) cases of
                                                                        Nothing -> error "Should not occur"
                                                                        Just e -> let (C x3 y3, e3) = (C (x+1) y ,e) in
                                                                            if appartient (C x3 y3, e3) v && e3 /= Mur  && appartient (C x3 y3, e3) xs
                                                                            then let xss = ([(C x3 y3, e3)]<>xs) in
                                                                                aux1 (Carte larg haut cases) (v<>[(C x y, caase)]) (xss<>[(C x y, caase)]) (C x6 y6) 
                                                                            else  case M.lookup (C (x-1) y) cases of
                                                                                        Nothing -> error "Should not occur"
                                                                                        Just e -> let (C x4 y4, e4) = (C (x-1) y ,e) in
                                                                                            if appartient (C x4 y4, e4) v && e4 /= Mur  && appartient (C x4 y4, e4) xs
                                                                                            then let xss = ([(C x4 y4, e4)]<>xs) in
                                                                                                aux1 (Carte larg haut cases) (v<>[(C x y, caase)]) (xss<>[(C x y, caase)]) (C x6 y6)
                                                                                            else aux1 (Carte larg haut cases) ([(C x y, caase)]<>v) xs (C x6 y6) 



makeOrder :: Coord -> Int -> Int -> Ordre
makeOrder crd@(C x y) x2 y2  
  | (x-x2, y-y2) == ((-1),0) = Model.E 
  | (x-x2, y-y2) == (1,0) = O 
  | (x-x2, y-y2) == (0,(-1)) = S  
  | (x-x2, y-y2) == (0,1) = N  

makeCoord :: Ordre -> Coord -> Coord 
makeCoord ord (C x y) = case ord of 
  N -> C x (y-1)  
  S -> C x (y+1)  
  O -> C (x-1) y  
  Model.E -> C (x+1) y  

--random weighted pick the order and make a move altering the model

randomCumulate :: [(Int,Ordre)] -> Int -> Int -> (Int,Ordre) 
randomCumulate [] _ _ = error "empty Rnadom cumulate never occurs"
randomCumulate (x:xs) p acc = 
  let acc' = acc + (fst x) in 
   if (p <= acc') then x else randomCumulate xs p acc'

pickRandomWeighted :: [(Int,Ordre)] -> StdGen -> Ordre 
pickRandomWeighted l gen =
  let totalweight = L.foldl (\b (v,_)->b+v) 0 l in
  let (p,_) = randomR (1,totalweight) gen in
  snd (randomCumulate l p 0)

decide :: [(Int, Ordre)] -> Modele -> Entite-> Coord -> Modele
decide l m@(Model c env g lg k) ent cord =
  if (length l == 0 )then m else
  let ord = pickRandomWeighted l g in
  let remEnv = M.delete cord (contenu_envi env) in  
  let movedEnv = M.insert (makeCoord ord cord) [ent] remEnv in
    Model c (Envi movedEnv) g lg k
  

post_prevoit:: Modele -> Coord -> Bool 
post_prevoit m c = undefined --il y a bien des action proposés (dans une situation correcte)

--see through map and envi where i can go
prevoit:: Modele -> Coord -> [(Int, Ordre)]
prevoit m@(Model c e g _ _ ) crd@(C x y) = 
  let toLookUp = [(x+1,y),(x-1,y),(x,y-1),(x,y+1)] in --r l u d 
  let cases = catMaybes $ L.map (\(cx,cy) -> (getCaseCoord c cx cy) ) toLookUp in 
  let passable_carte = L.filter (\(_,(C ax by)) -> isTraversable c ax by ) cases in 
  let passable_env = L.filter (\(_,crd2) -> franchissable_env crd2 e False ) passable_carte in 
  let weights = getNRandom [1..4] (length passable_env) g False in 
  let ordres = L.map (\(_,(C x1 y1)) -> makeOrder crd x1 y1 ) passable_env in 
    zip weights ordres 
  
bouge :: Modele -> Entite -> Coord -> CDouble ->Modele
bouge m ent@(Mob id hp st) coord time = 
  if( (round(time - st) `mod` 3 == 0) && (time - st)-((fromIntegral $ round(time - st))::CDouble) <  0.01 ) -- a step every 3 seconds (normally)
    then decide (prevoit m coord) m ent{starting_time = st +2} coord
    else m

post_bouge ::  Modele -> Entite -> [(Int, Ordre)] -> Bool 
post_bouge m e l = undefined --on bouge bien dans une des directions proposés via "decide"

stepMobs ::Modele -> CDouble -> Modele 
stepMobs m@(Model c e g l k ) time = 
  let mobs = M.filter (\x -> isMob $ head x) (contenu_envi e) in --TODO multiple entit one case
  M.foldlWithKey (\md crd ent  -> (bouge md (head ent) crd) time) m mobs


openDoorGenerique :: Modele -> Coord -> Modele
openDoorGenerique m@(Model c e g l k ) (C x1 y1) = 
  let players = M.filter (\x -> isPlayer $ head x) (contenu_envi e) in
  let liste = M.toAscList(players) in 
  let (C x y, ent) = head liste in
  let a = fromJust $ getCase c (x+x1) (y+y1) in
    if a == Porte EO Fermee || a == Porte NS Fermee
    then let remCar = M.delete (C (x+x1) (y+y1)) (carte_contenu c) in  
          if a == Porte EO Fermee
          then let movedCar = M.insert (C (x+x1) (y+y1)) (Porte EO Ouverte) remCar in
                      Model (Carte (cartel c) (carteh c) movedCar) e g l k
          else let movedCar = M.insert (C (x+x1) (y+y1)) (Porte NS Ouverte) remCar in
                      Model (Carte (cartel c) (carteh c) movedCar) e g l k
    else m 

post_openDoor :: Modele -> Coord ->Bool
post_openDoor m c = undefined --verifier la que la porte est bien ouverte (s'il y a une porte)


pre_moveGenerique :: Modele -> Coord -> Bool --On vérifie si les coordonnés sont toujours sur la carte
pre_moveGenerique m@(Model c@(Carte l h contenu_carte) e g lg k ) (C x1 y1) =   
  let (C x y, entites) = getPlayer e in
  let (C x2 y2) = C (x+x1) (y+y1) in
     isTraversable c x2 y2 && franchissable_env (C x2 y2) e True && (x2 < l) && (y2 < h) && (x2 > 0) && (y2 > 0)



moveGenerique :: Modele -> Coord -> Modele 
moveGenerique m@(Model c e g l k ) (C x1 y1) = 
  let players = M.filter (\x -> isPlayer $ head x) (contenu_envi e) in
  let liste = M.toAscList(players) in 
  let (C x y, ent) = head liste in
  let a = fromJust $ getCase c (x+x1) (y+y1) in
    if isTraversable c (x+x1) (y+y1) && franchissable_env (C (x+x1) (y+y1)) e True
              then  let remEnv = M.delete (C x y) (contenu_envi e) in  
                    let movedEnv = M.insert (C (x+x1) (y+y1)) ent remEnv in
                    Model c (Envi movedEnv) g l k
              else m 

post_moveGenerique :: Modele -> Coord -> Bool --On vérifie si le joueur à bien fini sur ces coordonnés
post_moveGenerique m@(Model c@(Carte l h contenu_carte) e g lg k ) (C x1 y1) =
  case M.lookup (C x1 y1) (contenu_envi e) of
    Nothing -> False 
    Just ent -> isPlayer(head ent)


pre_interact_object ::  Modele -> Coord -> Bool 
pre_interact_object m c = undefined -- il y a bien un objet  

interactObject :: Modele -> Coord -> Modele
interactObject m@(Model c e g l k ) crd@(C x y) =
  let (C x1 y1, ent) = getPlayer e  in 
  let cordObject =  C (x1+x) (y1+y) in case trouve_env_Cord e cordObject of 
    Nothing -> m 
    Just Treasure -> let trPlLess =  (rmv_coor_envi (C x1 y1) (rmv_coor_envi cordObject e))  in
                let updatedPlayer =  ajout_env (C x1 y1, (head ent){hasTreasure=True}) trPlLess in 
                Model c updatedPlayer g l k 
    Just (Mob idMob hp timeMob) -> let enviNoHitMob = (rmv_coor_envi cordObject e) in 
      if(hp - 25 < 0)
        then Model c enviNoHitMob g l k --mob has died
        else Model c (ajout_env (cordObject,(Mob idMob (hp-25) timeMob)) enviNoHitMob) g l k --placing back the mob that has been hurt
    Just _ -> m 

post_interact_object ::  Modele -> Coord ->Entite -> Bool 
post_interact_object m c = undefined -- on a bien intéragit avec l'objet : pas de tresor ou mob blessé. 

interactObjectsEnvi :: Modele -> Modele 
interactObjectsEnvi m =   L.foldl (\modele coord -> interactObject modele coord) m [(C (-1) 0),(C 1 0),(C 0 (-1)),(C 0 1)]

stepPlayer :: Modele -> Keyboard -> Modele
stepPlayer m kbd =
  let new_mo =if S.member KeycodeZ kbd then moveGenerique m (C 0 (-1))
              else if S.member KeycodeS kbd then moveGenerique m (C 0 1) 
              else if S.member KeycodeQ kbd then moveGenerique m (C (-1) 0)
              else if S.member KeycodeD kbd then moveGenerique m (C 1 0)
              else if S.member KeycodeE kbd then L.foldl ( \coord modele -> openDoorGenerique coord modele  ) m ([(C (-1) 0),(C 1 0),(C 0 (-1)),(C 0 1)])
              else if S.member KeycodeR kbd then interactObjectsEnvi m 
              else m
    in
      new_mo


