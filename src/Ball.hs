module Ball where

import Constants
import Graphics.Gloss

type Ball = (Point, Point)

type Balls = ([Ball], Int)

drawball ((x, y), _) = Color green $ Translate x y $ Circle ballSize

move ((px, py), (dx, dy)) = ((px + dx, py + dy), (dx, dy))

insidewalls ((px, py), (_, dy)) = (px >= 0 && px < sw && py <= 0 && py > - sh) || (dy > 0 && py < - sh)

clampwalls ((px, py), (dx, dy)) = ((npx, npy), (ndx, ndy))
  where
    wx x y
      | x < 0 && y > - h = (- x, - dx)
      | x > w && y > - h = (2 * w - x, - dx)
      | otherwise = (x, dx)
    wy y
      | y > 0 = (- y, - dy)
      | otherwise = (y, dy)
    (npx, ndx) = wx px py
    (npy, ndy) = wy py
    w = fromIntegral $ fst size
    h = fromIntegral $ snd size
