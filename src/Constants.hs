module Constants where

import Graphics.Gloss (Point)

type Ran = [Int]

size :: (Int, Int)
size = (400, 400)

sw :: Float
sw = fromIntegral $ fst size

sh :: Float
sh = fromIntegral $ snd size

ballSize :: Float
ballSize = bh / 4

brickSize :: (Int, Int)
brickSize = (40, 20)

bw :: Float
bw = fromIntegral $ fst brickSize

bh :: Float
bh = fromIntegral $ snd brickSize

px = sw / 2

py = 2 * ballSize - sh

out :: Point
out = (-100000, -100000)
