module Ball where

import Constants
import Graphics.Gloss

type Ball = (Point, Point)

type Balls = ([Ball], Int)

v :: Int
v = 5

drawball ((x, y), _) = Color green $ Translate x y $ Circle ballSize

move ((px, py), (dx, dy)) = ((px + dx, py + dy), (dx, dy))

insidewalls ((px, py), (_, dy)) = px >= 0 && px < w && (py <= 0 && py > - h || dy > 0)

clampwalls ((px, py), (dx, dy)) = ((npx, npy), (ndx, ndy))
  where
    wx x
      | x < 0 = (- x, - dx)
      | x > w = (2 * w - x, - dx)
      | otherwise = (x, dx)
    wy y
      | y > 0 = (- y, - dy)
      | otherwise = (y, dy)
    (npx, ndx) = wx px
    (npy, ndy) = wy py
    w = fromIntegral $ fst size
    h = fromIntegral $ snd size
