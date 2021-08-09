{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnboxedTuples #-}

module World where

import Ball
import Brick
import Constants
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Lens.Micro
import Lens.Micro.TH

data Status = Aiming Point | Playing | Dead

data World = World
  { _stage :: !Int,
    _score :: !Int,
    _ran :: !Ran,
    _balls :: ![Ball],
    _maxBalls :: !Int,
    _bricks :: !Bricks,
    _status :: !Status
  }

makeLenses ''World

isAiming :: Status -> Bool
isAiming (Aiming _) = True
isAiming _ = False

isPlaying :: Status -> Bool
isPlaying Playing = True
isPlaying _ = False

topleft :: [Picture] -> Picture
topleft = Translate (- sw / 2) (sh / 2) . Pictures

initWorld :: Ran -> World
initWorld ran0 = World 1 0 nran [] 1000 nbricks (Aiming out)
  where
    (nran, nbricks) = go 8 $ newBricks 1 ran0
    go 0 rb = rb
    go i (ran1, bricks1) = go (i -1) $ newRow i ran1 bricks1

draw :: World -> Picture
draw w = case w ^. status of
  Playing -> topleft $ tscore : lim : (drawball <$> w ^. balls) <> nbricks
  Aiming (x, y) -> topleft $ tscore : lim : nbricks <> mouseinside (x, y)
  Dead -> Color red $ Scale 0.1 0.1 $ Text ("Score: " <> show (w ^. score) <> ", stage: " <> show (w ^. stage))
  where
    mouseinside (x, y) = drawball (Ball (px, py) (0, 0)) : [Line [(px, py), (px + x, y - sh / 2)] | x >= - sw / 2 && x <= sw / 2 && y <= sh / 2 && y >= bh - sh / 2]
    nbricks = drawbrick <$> w ^. bricks
    tscore = Translate 5 (5 - sh) $ Scale 0.1 0.1 $ Text ("s: " <> show (w ^. score) <> "   b: " <> show (w ^. maxBalls))
    lim = Color red $ Line [(0, bh - sh), (sw, bh - sh)]

input :: Event -> World -> World
input (EventMotion (x, y)) w | isAiming (w ^. status) = w & status .~ Aiming (x, y)
input (EventKey (MouseButton LeftButton) Down _ (x, y)) w | isAiming (w ^. status) && y > bh - sh / 2 = w & status .~ Playing & balls .~ nballs
  where
    nballs = [Ball (px - i * d * dx, py - i * d * dy) (dx, dy) | i <- fromIntegral <$> [0 .. w ^. maxBalls -1]]
    d = 4 * ballSize * 0.1
    (dx, dy) = normalize (x, y - sh / 2 - py)
    normalize (a, b) = let l = sqrt (a * a + b * b) in (a / l, b / l)
input _ w = w

update :: Float -> World -> World
update _ = go v
  where
    v = 10 :: Int
    go 0 w = w
    go i w = case update' w of
      w' | isAiming (w' ^. status) -> w'
      w' -> go (i -1) w'

update' :: World -> World
update' w
  | null (w ^. balls) && isPlaying (w ^. status) =
    if reachedBottom (w ^. bricks)
      then w & status .~ Dead
      else
        let (nran, nbricks) = newRow n (w ^. ran) (w ^. bricks)
            n = w ^. stage + 1
         in w & bricks .~ nbricks & status .~ Aiming out & stage .~ n & ran .~ nran
update' w = case w ^. status of
  Playing -> w & balls .~ nballs & bricks .~ nbricks & maxBalls +~ dels & score +~ counts
    where
      balls' = filter insidewalls $ clampwalls . move <$> w ^. balls
      bricks' = w ^. bricks
      (nballs, counts, (nbricks, dels)) = if null balls' then (balls', 0, (bricks', 0)) else foldl foldballs ([], 0, (bricks', 0)) balls'
      foldballs (bs, c, (brs, d)) b = let (nb, c', nbrs) = foldl foldbricks (b, c, ([], d)) brs in (nb : bs, c', nbrs)
      foldbricks (b, c, brs) br = let (# nb, nbr, c' #) = collide b br in (nb, c + c', addbrick nbr brs)
      addbrick br (brs, d) = if alive br then (br : brs, d) else (brs, d + 1)
  _ -> w

collide :: Num c => Ball -> Brick -> (# Ball, Brick, c #)
collide b@(Ball (x, y) (dx, dy)) br =
  if inside b br
    then
      if ur
        then
          if dr
            then let dx' = abs dx in (# Ball (x + dx', y) (dx', dy), br & life -~ 1, 1 #)
            else let dy' = abs dy in (# Ball (x, y + dy') (dx, dy'), br & life -~ 1, 1 #)
        else
          if dr
            then let dy' = - abs dy in (# Ball (x, y + dy') (dx, dy'), br & life -~ 1, 1 #)
            else let dx' = - abs dx in (# Ball (x + dx', y) (dx', dy), br & life -~ 1, 1 #)
    else (# b, br, 0 #)
  where
    (bx, by) = br ^. pos
    ur = - bh * (x - bx) - bw * (y - by) < 0
    dr = bh * (x - bx) - bw * (y - (by - bh)) > 0
