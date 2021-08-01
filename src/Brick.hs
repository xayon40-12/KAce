{-# LANGUAGE TemplateHaskell #-}

module Brick where

import Constants
import Graphics.Gloss
import Lens.Micro.TH

data Brick = Brick
  { _life :: Int,
    _pos :: Point
  }
  deriving (Show)

makeLenses ''Brick

type Bricks = [Brick]

newBricks n = [Brick n (fromIntegral $ x * bw, 0) | x <- [0 .. s -1]]
  where
    s = fst size `div` bw
    bw = fst brickSize

reachedBottom [] = False
reachedBottom ((Brick _ (_, y)) : _) = - y >= h - bh

newRow n bricks = (count, nbricks <> newBricks n)
  where
    count = length bricks - length nbricks
    nbricks = filter alive (down <$> bricks)

down (Brick l (x, y)) = Brick l (x, y - bh)

alive (Brick l _) = l > 0

drawbrick (Brick life (x', y')) =
  Pictures
    [ Color blue $ Polygon [(x, y), (x + w', y), (x + w', y - h'), (x, y - h')],
      Color white $ Translate (x + 2) (y -12) $ Scale 0.1 0.1 $ Text (show life)
    ]
  where
    l = 2
    x = x' + l
    y = y' - l
    w' = bw - 2 * l
    h' = bh - 2 * l
