module Envi where

import qualified Data.Map.Strict as M
import Carte
import System.Random

data Entite = Mob {iden :: Int , pvie :: Int, starting_time :: Double  }
    | Player {iden :: Int, pvie :: Int}
    deriving (Eq,Show)

data Envi = Envi { contenu_envi :: M.Map Coord [ Entite ]} deriving (Show)



isPlayer :: Entite -> Bool 
isPlayer (Player _ _ ) = True
isPlayer _ = False


franchissable_env :: Coord -> Envi -> Bool
franchissable_env crd env= not $ M.member crd (contenu_envi env)

trouve_id :: Int -> Envi -> Maybe (Coord, Entite)
trouve_id = undefined

rm_env_id :: Int -> Envi -> Envi
rm_env_id = undefined

bouge_id :: Int -> Coord -> Envi -> Envi
bouge_id =undefined



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