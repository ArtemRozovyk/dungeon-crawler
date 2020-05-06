
module Envi where
import qualified Data.Map.Strict as M
import Carte



data Entite = Mob {iden :: Int , pvie :: Int}
    | Player {iden :: Int , pvie :: Int}
    deriving (Eq,Show)

isPlayer :: Entite -> Bool 
isPlayer (Player _ _ )= True
isPlayer _ = False


data Envi = Envi { contenu_envi :: M.Map Coord [ Entite ]} deriving (Show)



franchissable_env :: Coord -> Envi -> Bool
franchissable_env = undefined

trouve_id :: Int -> Envi -> Maybe (Coord, Entite)
trouve_id = undefined

rm_env_id :: Int -> Envi -> Envi
rm_env_id = undefined

bouge_id :: Int -> Coord -> Envi -> Envi
bouge_id =undefined