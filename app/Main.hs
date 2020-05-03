module Main where

import System.IO

import Carte

carteFromFile :: FilePath -> IO Carte
carteFromFile file = do 
    fd <- openFile file ReadMode
    str <- hGetContents fd
    return (read str :: Carte)

carteToFile :: Carte -> FilePath -> IO ()
carteToFile carte file = do 
    writeFile file (show carte)
    return ()

main :: IO ()
main = undefined
