{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Brick where

import Constants
import Graphics.Gloss hiding (color)
import Lens.Micro
import Lens.Micro.TH

data Brick = Brick
  { _brickLife :: !Int,
    _brickPos :: !Point,
    _brickColor :: !Color
  }
  deriving (Show)

makeFields ''Brick

type Bricks = [Brick]

newBricks :: Int -> Ran -> (Ran, Bricks)
newBricks n ran = (ran1, [Brick n (x * bw, 0) (makeColorI r g b 255) | (x, (r, g, b)) <- zip [0 .. s -1] cols])
  where
    s = sw / bw
    (cols, ran1) = go ran (floor s)
    go :: Ran -> Int -> ([(Int, Int, Int)], Ran)
    go r 0 = ([], r)
    go (r : g : b : rs) i = let (c, rs1) = go rs (i -1) in ((mod r 256, mod g 256, mod b 256) : c, rs1)
    go _ _ = error "The list of random numbers should be infinit in a call for newBricks."

reachedBottom :: Bricks -> Bool
reachedBottom = foldr (\b -> (||) (- (b ^. pos . _2) >= sh - 2 * bh)) False

newRow :: Int -> Ran -> Bricks -> (Ran, Bricks)
newRow n ran bricks = let (ran2, knbricks) = kill ran1 nbricks [] in (ran2, fbricks <> knbricks)
  where
    f x = x * x
    fbricks = filter alive (down <$> bricks)
    (ran1, nbricks) = newBricks (f n) ran
    kill [] bs nbs = ([], bs <> nbs)
    kill rs [] nbs = (rs, nbs)
    kill (r : rs) (b : bs) nbs = if r `mod` p == 0 then kill rs bs nbs else kill rs bs (b : nbs)
    p = 2

down :: Brick -> Brick
down b = b & pos . _2 -~ bh

alive :: Brick -> Bool
alive b = b ^. life > 0

drawbrick :: Brick -> Picture
drawbrick b =
  Pictures
    [ Color (b ^. color) $ Polygon [(x, y), (x + w', y), (x + w', y - h'), (x, y - h')],
      Color white $ Translate (x + 2) (y -12) $ Scale 0.1 0.1 $ Text (show $ b ^. life)
    ]
  where
    l = 1
    (x', y') = b ^. pos
    x = x' + l
    y = y' - l
    w' = bw - 2 * l
    h' = bh - 2 * l
