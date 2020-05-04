{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Map.Strict as Mp

import System.IO

import Carte

import Control.Monad (unless,foldM,mapM_,guard)
import Control.Concurrent (threadDelay)

import Data.Set (Set)
import qualified Data.Set as Set

import Data.List (foldl')

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

import Model (GameState)
import qualified Model as M


carteFromFile :: FilePath -> IO Carte
carteFromFile file = do 
    fd <- openFile file ReadMode
    str <- hGetContents fd
    return (read str :: Carte)
    
carteFromFileShow :: FilePath -> IO ()
carteFromFileShow file = do 
    fd <- openFile file ReadMode
    str <- hGetContents fd
    putStrLn str

carteToFile :: Carte -> FilePath -> IO ()
carteToFile carte file = do 
    writeFile file (show carte)
    return ()

loadBackground :: Renderer-> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadBackground rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId "background") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "background") (S.mkArea 0 0 640 480)
  let smap' = SM.addSprite (SpriteId "background") sprite smap
  return (tmap', smap')

loadPerso :: Renderer-> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadPerso rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId "perso") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "perso") (S.mkArea 0 0 100 100)
  let smap' = SM.addSprite (SpriteId "perso") sprite smap
  return (tmap', smap')

loadVirus :: Renderer-> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadVirus rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId "virus") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "virus") (S.mkArea 0 0 32 32)
  let smap' = SM.addSprite (SpriteId "virus") sprite smap
  return (tmap', smap')


loadGeneric :: Renderer-> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadGeneric rdr path tmap smap  = do
  let name = takeWhile (/= '.') path 
  tmap' <- TM.loadTexture rdr ("assets/used/"++path) (TextureId name) tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId name) (S.mkArea 0 0 48 48)
  let smap' = SM.addSprite (SpriteId name) sprite smap
  return (tmap', smap')  


toLoad = ["brick_brown.png","closed_door_eo.png",
    "closed_door_ns.png", "entrance.png","exit.png",
    "open_door_eo.png","open_door_ns.png"]; 
tiles = ["X","|","-","E","S","/","\\"]
mapTiles = Mp.fromList (zip tiles (map (takeWhile (/= '.')) toLoad))

fetchSingleSprite :: Maybe String -> SpriteMap -> Int ->Int -> Sprite
fetchSingleSprite Nothing _ _ _ = error "should not occur"
fetchSingleSprite (Just name) smap x y = 
  (S.moveTo (SM.fetchSprite (SpriteId name) smap) (fromIntegral x) (fromIntegral y))

--fetches sprites that are already positionned correctly
fetchSpritesFromCarte :: Carte ->SpriteMap-> [Sprite]
fetchSpritesFromCarte carte smap = 
  let contentList = Mp.toList(carte_contenu carte) in do
    (C x y,caze) <- contentList
    guard (Mp.member (show caze) mapTiles)
    return (fetchSingleSprite (Mp.lookup (show caze) mapTiles) smap (x*48) (y*48))


testCarte :: Carte -> [String]
testCarte carte =  let contentList = Mp.toList(carte_contenu carte) in do
    (C x y,caze) <- contentList
    guard (Mp.member (show caze) mapTiles)
    return ( Mp.findWithDefault "z" (show caze) mapTiles )

main :: IO ()
main = do
  initializeAll
  window <- createWindow "Franchir l'Oubliette" $ defaultWindow { windowInitialSize = V2 480 480 }
  renderer <- createRenderer window (-1) defaultRenderer
  (tmap0,smap0) <- (return $ (TM.createTextureMap,SM.createSpriteMap))
  (tmap0',smap0') <- foldM (\(tmp,smp) path -> loadGeneric renderer path tmp smp ) (tmap0,smap0) toLoad

  carte <-carteFromFile "exemple"
  pure (putStrLn (show carte))
  -- chargement de l'image du fond
  (tmap, smap) <- loadBackground renderer "assets/background.jpg" tmap0' smap0'
  -- initialisation de l'état du jeu
  gameState <- M.initGameState
  -- initialisation de l'état du clavier
  let kbd = K.createKeyboard
  --putStrLn $ concat $ testCarte carte

  -- lancement de la gameLoop
  gameLoop 60 renderer tmap smap kbd gameState carte

gameLoop :: (RealFrac a, Show a) => a -> Renderer -> TextureMap -> SpriteMap -> Keyboard -> GameState -> Carte -> IO ()
gameLoop frameRate renderer tmap smap kbd gameState carte = do
  startTime <- time
  events <- pollEvents
  let kbd' = K.handleEvents events kbd
  clear renderer
  --- display background
  S.displaySprite renderer tmap (SM.fetchSprite (SpriteId "background") smap)

  --- display carte 
  
  mapM_ (S.displaySprite renderer tmap) (fetchSpritesFromCarte carte smap)
 
   
 
  present renderer
  endTime <- time
  let refreshTime = endTime - startTime
  let delayTime = floor (((1.0 / frameRate) - refreshTime) * 1000)
  threadDelay $ delayTime * 1000 -- microseconds
  endTime <- time
  let deltaTime = endTime - startTime
  -- putStrLn $ "Delta time: " <> (show (deltaTime * 1000)) <> " (ms)"
  -- putStrLn $ "Frame rate: " <> (show (1 / deltaTime)) <> " (frame/s)"
  --- update du game state
  let gameState' = M.gameStep gameState kbd' deltaTime
  ---
  unless (K.keypressed KeycodeEscape kbd') (gameLoop frameRate renderer tmap smap kbd' gameState' carte)
