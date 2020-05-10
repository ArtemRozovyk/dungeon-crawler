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

empty_state :: Carte-> StdGen -> Etat 
empty_state carte gen = 
    Tour 0 carte (Envi M.empty) gen M.empty ""

add_entity :: Etat -> Coord -> Entite -> Etat 
add_entity s c e =
    let id = (foldl max 0 (M.keys $ obj_tour s ))+1 in
    let ent = if(not $ isObject $ e) then e{iden=id} else e in
    let ot = M.insert id ent (obj_tour s) in 
    if M.member c (contenu_envi (envi_tour s)) --there are already entities 
    then s { envi_tour= Envi $ M.adjust (++[ent]) c (contenu_envi (envi_tour s)), obj_tour=ot}
    else s { envi_tour= Envi $ M.insert c [ent] (contenu_envi (envi_tour s)), obj_tour=ot }

{-
etat_tour ::RealFrac a => Etat -> Keyboard -> a -> Etat
--call tour de toutes les entités obj_tour. win or loss? 

etat_tour etat kbd deltaTime = do 
    let model = Model (carte_tour etat) (gen_tour etat) "" kbd 
    foldl takeTurn model ()
-}






makeNEntities :: Int -> StdGen -> CDouble -> [Entite] 
makeNEntities n gen moment = 
    let timesl= getNRandom [0..11] n gen False in 
        map (\t -> Mob 0 100 (moment+t) ) timesl 




init_state :: Carte -> Int -> CDouble-> StdGen -> Etat -- (Envi, M.Map Int Entite )
init_state carte n moment gen =
    let (gen',g) = split gen in
    let entites = (Trap):(Treasure):(Treasure):(Treasure):(makeNEntities n g moment) in
    let emptyCases =  filter (\(_,c)-> c == Empty ) (M.toList $ carte_contenu carte) in 
    let places = getNRandom  emptyCases (length entites) gen' True in
    let mbs= foldl (\e (c,m) ->  add_entity e c m) (empty_state carte gen') (zip (map (\(c,_)-> c) places) entites) in
        let player = [Player 42 100 False] in
        mbs {envi_tour = Envi $ M.insert (getEntreeCoord carte) player (contenu_envi (envi_tour mbs)),
        obj_tour = M.insert 42 (head player) (obj_tour mbs)} 

initGameState :: Carte-> IO Etat
initGameState carte = do 
    gen  <- getStdGen::IO StdGen 
    let (v,_) = randomR (1::Integer,10::Integer ) gen 
    putStrLn $ show v
    moment <- time
    return $ init_state carte 2 moment gen  

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
    let modele = gameStep modele' kbd in
    let modeleF= stepMobs modele moment in
        Tour nt (carte modeleF) (envi modeleF) g2 ot ((log_m modeleF)++lgt)

    --let modeleF =stepPlayer modele' in 

     

