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
  
--see through map and envi where i can go
prevoit:: Modele -> Coord -> [(Int, Ordre)]
prevoit m@(Model c e g _ _ ) crd@(C x y) = 
  let toLookUp = [(x+1,y),(x-1,y),(x,y-1),(x,y+1)] in --r l u d 
  let cases = catMaybes $ L.map (\(cx,cy) -> (getCase2 c cx cy) ) toLookUp in 
  let passable_carte = L.filter (\(_,(C ax by)) -> isTraversable c ax by ) cases in 
  let passable_env = L.filter (\(_,crd2) -> franchissable_env crd2 e ) passable_carte in 
  let weights = getNRandom [1..4] (length passable_env) g False in 
  let ordres = L.map (\(_,(C x1 y1)) -> makeOrder crd x1 y1 ) passable_env in 
    zip weights ordres 
  
bouge :: Modele -> Entite -> Coord -> CDouble ->Modele
bouge m ent@(Mob id hp st) coord time = 
  if( (round(time - st) `mod` 3 == 0) && (time - st)-((fromIntegral $ round(time - st))::CDouble) <  0.01 ) -- a step every 2 seconds (normally)
    then decide (prevoit m coord) m ent{starting_time = st +2} coord
    else m

stepMobs ::Modele -> CDouble -> Modele 
stepMobs m@(Model c e g l k ) time = 
  let mobs = M.filter (\x -> isMob $ head x) (contenu_envi e) in --TODO multiple entit one case
  M.foldlWithKey (\md crd ent  -> (bouge md (head ent) crd) time) m mobs

stepPlayer ::Modele -> Modele 
stepPlayer m@(Model c e g l k ) = undefined

openDoor1 :: Modele -> Modele 
openDoor1 m@(Model c e g l k ) =
  let players = M.filter (\x -> isPlayer $ head x) (contenu_envi e) in
  let liste = M.toAscList(players) in 
  let (C x y, ent) = head liste in
  case (M.lookup (C (x+1) y) (carte_contenu c)) of
    Nothing -> error "Mon erreur2"
    Just a -> if a == Porte EO Fermee
              then let remCar = M.delete (C (x+1) y) (carte_contenu c) in  
                   let movedCar = M.insert (C (x+1) y) (Porte EO Ouverte) remCar in
                      Model (Carte (cartel c) (carteh c) movedCar) e g l k
              else m

openDoor2 :: Modele -> Modele 
openDoor2 m@(Model c e g l k ) =
  let players = M.filter (\x -> isPlayer $ head x) (contenu_envi e) in
  let liste = M.toAscList(players) in 
  let (C x y, ent) = head liste in
  case (M.lookup (C (x-1) y) (carte_contenu c)) of
    Nothing -> error "Mon erreur2"
    Just a -> if a == Porte EO Fermee
              then let remCar = M.delete (C (x-1) y) (carte_contenu c) in  
                   let movedCar = M.insert (C (x-1) y) (Porte EO Ouverte) remCar in
                      Model (Carte (cartel c) (carteh c) movedCar) e g l k
              else m

openDoor3 :: Modele -> Modele 
openDoor3 m@(Model c e g l k ) =
  let players = M.filter (\x -> isPlayer $ head x) (contenu_envi e) in
  let liste = M.toAscList(players) in 
  let (C x y, ent) = head liste in
  case (M.lookup (C x (y+1)) (carte_contenu c)) of
    Nothing -> error "Mon erreur2"
    Just a -> if a == Porte NS Fermee
              then let remCar = M.delete (C x (y+1)) (carte_contenu c) in  
                   let movedCar = M.insert (C x (y+1)) (Porte NS Ouverte) remCar in
                      Model (Carte (cartel c) (carteh c) movedCar) e g l k
              else m

openDoor4 :: Modele -> Modele 
openDoor4 m@(Model c e g l k ) =
  let players = M.filter (\x -> isPlayer $ head x) (contenu_envi e) in
  let liste = M.toAscList(players) in 
  let (C x y, ent) = head liste in
  case (M.lookup (C x (y-1)) (carte_contenu c)) of
    Nothing -> error "Mon erreur2"
    Just a -> if a == Porte NS Fermee
              then let remCar = M.delete (C x (y-1)) (carte_contenu c) in  
                   let movedCar = M.insert (C x (y-1)) (Porte NS Ouverte) remCar in
                      Model (Carte (cartel c) (carteh c) movedCar) e g l k
              else m




moveUp :: Modele -> Modele
moveUp m@(Model c e g l k ) =
  let players = M.filter (\x -> isPlayer $ head x) (contenu_envi e) in
  let liste = M.toAscList(players) in 
  let (C x y, ent) = head liste in
  let a = fromJust $ getCase c x (y-1) in -- erreur est lancé si Nothing, mais n'arrive jamais car carte est entrouré des murs. 
    if isTraversable c x (y-1) && franchissable_env (C x (y-1)) e && a /= (Porte NS Fermee) && a /= (Porte EO Fermee)
              then  let remEnv = M.delete (C x y) (contenu_envi e) in  
                    let movedEnv = M.insert (C x (y-1)) ent remEnv in
                    Model c (Envi movedEnv) g l k
              else m 



moveDown :: Modele -> Modele
moveDown m@(Model c e g l k ) =
  let players = M.filter (\x -> isPlayer $ head x) (contenu_envi e) in
  let liste = M.toAscList(players) in 
  let (C x y, ent) = head liste in
  case (M.lookup (C x (y+1)) (carte_contenu c)) of
    Nothing -> error "Mon erreur"
    Just a -> if isTraversable c x (y+1) && franchissable_env (C x (y+1)) e && a /= (Porte NS Fermee) && a /= (Porte EO Fermee)
              then  let remEnv = M.delete (C x y) (contenu_envi e) in  
                    let movedEnv = M.insert (C x (y+1)) ent remEnv in
                    Model c (Envi movedEnv) g l k
              else m 

moveLeft :: Modele -> Modele
moveLeft m@(Model c e g l k ) =
  let players = M.filter (\x -> isPlayer $ head x) (contenu_envi e) in 
  let liste = M.toAscList(players) in 
  let (C x y, ent) = head liste in
  case (M.lookup (C (x-1) y) (carte_contenu c)) of
    Nothing -> error "Mon erreur"
    Just a -> if isTraversable c (x-1) y && franchissable_env (C (x-1) y) e && a /= (Porte NS Fermee) && a /= (Porte EO Fermee)
              then  let remEnv = M.delete (C x y) (contenu_envi e) in  
                    let movedEnv = M.insert (C (x-1) y) ent remEnv in
                    Model c (Envi movedEnv) g l k
              else m 

moveRight :: Modele -> Modele
moveRight m@(Model c e g l k ) =
  let (C x y, ent) = getPlayer e in
  case (M.lookup (C (x+1) y) (carte_contenu c)) of
    Nothing -> error "Mon erreur"
    Just a -> if isTraversable c (x+1) y && franchissable_env (C (x+1) y) e && a /= (Porte NS Fermee) && a /= (Porte EO Fermee)
              then  let remEnv = M.delete (C x y) (contenu_envi e) in  
                    let movedEnv = M.insert (C (x+1) y) ent remEnv in
                    Model c (Envi movedEnv) g l k
              else m 


takeTreasure :: Modele -> Coord -> Modele
takeTreasure m@(Model c e g l k ) crd@(C x y) = 
  let (C x y, ent) = getPlayer e  in
  if (fromJust $ trouveCord e crd ) == Treasure
              then 
                let treasureLess = Envi $ M.fromList $ rmv_coor crd (M.toList $ contenu_envi e) in
                let updatedPlayer =  ajout (C x y, (head ent){hasTreasure=True}) (rm_env_id 42 e) in 
                Model c updatedPlayer g l k 
                else m

            



gameStep :: Modele -> Keyboard -> Modele
gameStep m kbd =
  let new_mo =if S.member KeycodeZ kbd then moveUp m
              else if S.member KeycodeS kbd then moveDown m
              else if S.member KeycodeQ kbd then moveLeft m
              else if S.member KeycodeD kbd then moveRight m
              else if S.member KeycodeE kbd then openDoor4(openDoor3(openDoor2(openDoor1 m))) -- rammaserTresor(frapperMob(opendoor))
              else if S.member KeycodeR kbd 
                then L.foldl ( \ modele coord -> takeTreasure modele coord   ) m [(C (-1) 0),(C 1 0),(C 0 (-1)),(C 0 1)]

              else m
    in
      new_mo
    


{-
data Etat = Perdu
    | Gagne
    | Tour {
    num_tour :: Int,
    carte_tour :: Carte,
    envi_tour :: Envi,
    gen_tour :: StdGen,
    obj_tour :: (M.Map Int Entite),
    log :: String}

data GameState = GameState { persoX :: Int
                           , persoY :: Int
                           , virusX :: Int
                           , virusY :: Int
                           , speed :: Int }
  deriving (Show)

initGameState :: IO GameState
initGameState = do
    vX <- randomRIO(40,290)
    vY <- randomRIO(40,290)
    pure $ GameState 200 200 vX vY  3

moveLeft :: GameState -> GameState
moveLeft gs@(GameState px _ _ _ sp) | px > 0 = gs { persoX = px - sp }
                                | otherwise = gs

moveRight :: GameState -> GameState
moveRight gs@(GameState px _ _ _ sp) | px < 320 = gs { persoX = px + sp }
                                 | otherwise = gs
                              
moveUp :: GameState -> GameState
moveUp gs@(GameState _ py _ _ sp) | py > 0 = gs { persoY = py - sp }
                              | otherwise = gs

moveDown :: GameState -> GameState
moveDown gs@(GameState _ py _ _ sp) | py < 320 = gs { persoY = py + sp }
                                | otherwise = gs
gameStep :: RealFrac a => GameState -> Keyboard -> a -> GameState
gameStep gstate kbd deltaTime =
  -- A MODIFIFIER
  let new_gs =if S.member KeycodeZ kbd then moveUp gstate
              else if S.member KeycodeS kbd then moveDown gstate
              else if S.member KeycodeQ kbd then moveLeft gstate
              else if S.member KeycodeD kbd then moveRight gstate
              else gstate
    in
      end_check new_gs

end_check :: GameState -> GameState
end_check gs@(GameState px py vx vy sp) = 
  if(abs(px-vx)<50 && abs(py-vy)<50)
  then GameState px py (-1) (-1) sp
  else gs
-}


