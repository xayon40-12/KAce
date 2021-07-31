module Main where

import Graphics.Gloss
import System.Random
import World

window :: Display
window = InWindow "KAce" size (10, 10)

main :: IO ()
main = do
  g <- getStdGen
  let ran = randoms g
  play window white 60 (initWorld ran) draw input update
