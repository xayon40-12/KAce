{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Ball where

import Brick
import Constants
import Graphics.Gloss
import Lens.Micro.TH

data Ball = Ball
  { _ballPos :: !Point,
    _ballDir :: !Point
  }

makeFields ''Ball

type Balls = ([Ball], Int)

drawball :: Ball -> Picture
drawball (Ball (x, y) _) = Color green $ Translate x y $ ThickCircle (ballSize / 2) ballSize

move :: Ball -> Ball
move (Ball (x, y) (dx, dy)) = Ball (x + dx, y + dy) (dx, dy)

insidewalls :: Ball -> Bool
insidewalls (Ball (x, y) (_, dy)) = (x >= 0 && x < sw && y <= 0 && y > - sh) || (dy > 0 && y < - sh)

clampwalls :: Ball -> Ball
clampwalls (Ball (bx, by) (dx, dy)) = Ball (npx, npy) (ndx, ndy)
  where
    wx x y
      | x < 0 && y > - h = (- x, - dx)
      | x > w && y > - h = (2 * w - x, - dx)
      | otherwise = (x, dx)
    wy y
      | y > 0 = (- y, - dy)
      | otherwise = (y, dy)
    (npx, ndx) = wx bx by
    (npy, ndy) = wy by
    w = fromIntegral $ fst size
    h = fromIntegral $ snd size

inside :: Ball -> Brick -> Bool
inside (Ball (x, y) _) (Brick _ (bx, by) _) = y >= by - bh && y <= by && x >= bx && x <= bx + bw
