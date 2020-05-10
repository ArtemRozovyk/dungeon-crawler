{-# LANGUAGE OverloadedStrings #-}

module Main where

import Carte
import Control.Monad (unless,foldM,mapM_)
import Control.Concurrent (threadDelay)
import qualified Data.Map.Strict as Mp

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

import Model ()
import qualified Model as M
import System.Random
import State

-- 
{-
toModel k e@(Tour _ c env g o l)= Model c env g "" k
 do kart <- carteFromFile "exemple";   s <- initGameState kart ; let toModel k e@(Tour _ c env g o l)= Model c env g "" k in let m = toModel K.createKeyboard  s in  putStrLn $ show $ contenu_envi $envi m ; return () 

-}


toLoad = ["brick_brown.bmp","closed_door_eo.bmp",
    "closed_door_ns.bmp", "entrance.bmp","exit.bmp",
    "open_door_eo.bmp","open_door_ns.bmp","player.bmp","monster.bmp"]; 
tiles = ["X","|","-","E","S","/","\\","p","m"]
mapTiles = Mp.fromList (zip tiles (map (takeWhile (/= '.')) toLoad))

loadGeneric :: Renderer-> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadGeneric rdr path tmap smap  = do
  let name = takeWhile (/= '.') path 
  let area = (if(name=="background") then (S.mkArea 0 0 480 480) else (S.mkArea 0 0 48 48))
  tmap' <- TM.loadTexture rdr ("assets/used/"++path) (TextureId name) tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId name) area
  let smap' = SM.addSprite (SpriteId name) sprite smap
  return (tmap', smap')  




main :: IO ()
main = do
  initializeAll
  carte <-carteFromFile "exemple"
  window <- createWindow "Franchir l'Oubliette" $ defaultWindow { windowInitialSize = V2 480 480 }
  renderer <- createRenderer window (-1) defaultRenderer 
  (tmap0,smap0) <- (return $ (TM.createTextureMap,SM.createSpriteMap))
  (tmap0',smap0') <- foldM (\(tmp,smp) path -> loadGeneric renderer path tmp smp ) (tmap0,smap0) toLoad
  (tmap, smap) <- loadGeneric renderer "background.bmp" tmap0' smap0'
  --putStrLn (show (carteh carte))
  -- chargement de l'image du fond
  -- initialisation de l'état du jeu
  --gameState <- M.initGameState
  -- initialisation de l'état du clavier
  let kbd = K.createKeyboard
  --putStrLn $ concat $ testCarte carte
  -- lancement de la gameLoop
  gameState <- initGameState carte
  gameLoop 60 renderer tmap smap kbd gameState

gameLoop :: (RealFrac a, Show a) => a -> Renderer -> TextureMap -> SpriteMap -> Keyboard  -> Etat-> IO ()
gameLoop frameRate renderer tmap smap kbd gameState= do
  startTime <- time
  events <- pollEvents
  let kbd' = K.handleEvents events kbd
  clear renderer
  --- display background
  S.displaySprite renderer tmap (SM.fetchSprite (SpriteId "background") smap)
  --- display carte 
  mapM_ (S.displaySprite renderer tmap) (fetchSpritesFromCarte (carte_tour gameState) smap mapTiles)
  mapM_ (S.displaySprite renderer tmap) (fetchSpritesFromEnv gameState smap mapTiles)

  present renderer
  endTime <- time
  let refreshTime = endTime - startTime
  let delayTime = floor (((1.0 / frameRate) - refreshTime) * 1000)
  threadDelay $ delayTime * 1000 -- microseconds
  endTime <- time
  let deltaTime = endTime - startTime
  -- putStrLn $ "Delta time: " <> (show (deltaTime * 1000)) <> " (ms)"
  -- putStrLn $ "Frame rate: " <> (show (1 / deltaTime)) <> " (frame/s)"

  --putStrLn $ "Time : " <> (show endTime)

  --- update du game state
 
  let (gen,_) =split (gen_tour gameState)
  let (v,_) = randomR (1::Integer,10::Integer ) gen 
  --putStrLn $ show v
  let gameState' = etat_tour gameState kbd' endTime gen
  --let gs@(Tour nt carte' et gt ot lgt) = gameState'
  --let gameState' = M.gameStep gameState kbd' 
  let kbd'' = K.createKeyboard
  unless (K.keypressed KeycodeEscape kbd') (gameLoop frameRate renderer tmap smap kbd'' gameState')


--test :: IO ()
--test = do

