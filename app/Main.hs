module Main where

import Constants
import Graphics.Gloss
import System.Random
import World

window :: Display
window = InWindow "KAce" size (10, 10)

main :: IO ()
main = do
  g <- getStdGen
  let ran = randoms g
  play window white 300 (initWorld ran) draw input update
