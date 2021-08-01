module Constants where

size :: (Int, Int)
size = (400, 400)

w :: Float
w = fromIntegral $ fst size

h :: Float
h = fromIntegral $ snd size

ballSize :: Float
ballSize = bh / 4

brickSize :: (Int, Int)
brickSize = (40, 20)

bw :: Float
bw = fromIntegral $ fst brickSize

bh :: Float
bh = fromIntegral $ snd brickSize

px = w / 2

py = 2 * ballSize - h
