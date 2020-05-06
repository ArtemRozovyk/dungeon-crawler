module State where 

import Carte 
import Envi
import Model
import qualified Data.Map.Strict as M
import Control.Monad (unless,foldM,mapM_)

import System.Random
import Sprite (Sprite)
import qualified Sprite as S

import SpriteMap (SpriteMap, SpriteId (..))
import qualified SpriteMap as SM


data Etat = Perdu
    | Gagne
    | Tour { num_tour :: Int ,
    carte_tour :: Carte ,
    envi_tour :: Envi ,
    gen_tour :: StdGen ,
    obj_tour :: (M.Map Int Entite ),
    log :: String }




empty_state :: Carte-> StdGen -> Etat 
empty_state carte gen = 
    Tour 0 carte (Envi M.empty) gen M.empty ""

add_entity :: Etat -> Coord -> Entite -> Etat 
add_entity s c e =
    let id = (foldl max 0 (M.keys $ obj_tour s ))+1 in
    let ent = e{iden=id} in
    let ot = M.insert id ent (obj_tour s) in 
    if M.member c (contenu_envi (envi_tour s)) 
    then s { envi_tour= Envi $ M.adjust (++[ent]) c (contenu_envi (envi_tour s)),obj_tour=ot}
    else s { envi_tour= Envi $ M.insert c [ent] (contenu_envi (envi_tour s)), obj_tour=ot }



{-
etat_tour ::RealFrac a => Etat -> Keyboard -> a -> Etat
--call tour de toutes les entités obj_tour. win or loss? 

etat_tour etat kbd deltaTime = do 
    let model = Model (carte_tour etat) (gen_tour etat) "" kbd 
    foldl takeTurn model ()
-}


getRandom :: (Eq a) => [a] -> StdGen -> (a,StdGen)
getRandom lst gen = let res = randomR (0, (length lst)-1) gen in 
     ((lst !! fst res),snd res)

getNRandomAux :: (Eq a) => [a] -> Int-> StdGen-> [a] -> [a] 
getNRandomAux _ 0 gen acc = acc
getNRandomAux l n gen acc= 
    let (v,g) = getRandom l gen in
        if(v `elem` acc) 
            then getNRandomAux l n g acc
            else getNRandomAux l (n-1) g (v:acc)

getNRandom ::(Eq a) => [a] -> Int -> StdGen-> [a] 
getNRandom l n gen = if n<= length l then 
    getNRandomAux l n gen []
    else fail "Out of range"

init_state :: Carte -> StdGen -> Etat             --(Envi, M.Map Int Entite )
init_state carte gen =
    let entites = [Mob 0 100,Mob 0 100,Mob 0 100] in
    let emptyCases =  filter (\(_,c)-> c == Empty ) (M.toList $ carte_contenu carte) in 
    let places = getNRandom  emptyCases (length entites) gen in
    let mbs= foldl (\e (c,m) ->  add_entity e c m) (empty_state carte gen) (zip (map (\(c,_)-> c) places) [Mob 0 100,Mob 0 100,Mob 0 100]) in
        let player = [Player 42 100] in
        mbs { envi_tour = Envi $ M.insert (getEntreeCoord carte) player (contenu_envi (envi_tour mbs)),
        obj_tour = M.insert 42 (head player) (obj_tour mbs)  } 




initGameState :: Carte-> IO Etat
initGameState carte = do
    gen <- getStdGen 
    return $ init_state carte gen
    


fetchSpritesFromEnv :: Etat ->SpriteMap-> M.Map [Char] [Char] -> [Sprite]
fetchSpritesFromEnv state smap mapTiles = 
    let env = M.toList (contenu_envi $ envi_tour state) in do 
        (C x y,ents) <- env  --TODO multiple entities in single case
        let ent = head ents 
        return (fetchSingleSprite (M.lookup (if (isPlayer ent ) then "p" else "m") mapTiles) smap (x*48) (y*48))



