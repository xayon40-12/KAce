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
  let ran0 = randoms g
  play window white 60 (initWorld ran0) draw input update
