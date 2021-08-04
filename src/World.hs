{-# LANGUAGE TemplateHaskell #-}

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
    _balls :: !Balls,
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
initWorld ran0 = World 1 0 ran1 ([], 1) nbricks (Aiming out)
  where
    (ran1, nbricks) = newBricks 1 ran0

draw :: World -> Picture
draw w = case w ^. status of
  Playing -> topleft $ tscore : lim : (drawball <$> w ^. balls . _1) <> nbricks
  Aiming (x, y) -> topleft $ tscore : lim : nbricks <> mouseinside (x, y)
  Dead -> Color red $ Scale 0.1 0.1 $ Text ("Score: " <> show (w ^. score) <> ", stage: " <> show (w ^. stage))
  where
    mouseinside (x, y) = drawball (Ball (px, py) (0, 0)) : [Line [(px, py), (px + x, y - sh / 2)] | x >= - sw / 2 && x <= sw / 2 && y <= sh / 2 && y >= bh - sh / 2]
    nbricks = drawbrick <$> w ^. bricks
    tscore = Translate 5 (5 - sh) $ Scale 0.1 0.1 $ Text ("s: " <> show (w ^. score) <> "   b: " <> show (w ^. balls . _2))
    lim = Color red $ Line [(0, bh - sh), (sw, bh - sh)]

input :: Event -> World -> World
input (EventMotion (x, y)) w | isAiming (w ^. status) = w & status .~ Aiming (x, y)
input (EventKey (MouseButton LeftButton) Down _ (x, y)) w | isAiming (w ^. status) && y > bh - sh / 2 = w & status .~ Playing & balls . _1 .~ nballs
  where
    nballs = [Ball (px - i * d * dx, py - i * d * dy) (dx, dy) | i <- fromIntegral <$> [0 .. w ^. balls . _2 -1]]
    d = 4 * ballSize
    (dx, dy) = normalize (x, y - sh / 2 - py)
    normalize (a, b) = let l = sqrt (a * a + b * b) in (a / l, b / l)
input _ w = w

update :: Float -> World -> World
update _ w
  | null (w ^. balls . _1) && isPlaying (w ^. status) =
    if reachedBottom (w ^. bricks)
      then w & status .~ Dead
      else let (nran, nbricks) = newRow n (w ^. ran) (w ^. bricks) in w & bricks .~ nbricks & status .~ Aiming out & stage .~ n & ran .~ nran
  where
    n = w ^. stage + 1
update _ w = case w ^. status of
  Playing -> w & balls . _1 .~ nballs & bricks .~ nbricks & balls . _2 +~ count & score +~ dels
    where
      balls' = filter insidewalls $ clampwalls . move <$> w ^. balls . _1
      bricks' = w ^. bricks
      (nballs, dels, (nbricks, count)) = if null balls' then (balls', 0, (bricks', 0)) else foldl foldballs ([], 0, (bricks', 0)) balls'
      foldballs (bs, del, (brs, c)) b = let (nb, del', nbrs) = foldl foldbricks (b, del, ([], c)) brs in (nb : bs, del', nbrs)
      foldbricks (b, del, brs) br = let (nb, nbr, del') = collide b br in (nb, del + del', addbrick nbr brs)
      addbrick br (brs, c) = if alive br then (br : brs, c) else (brs, c + 1)
  _ -> w

collide :: Num c => Ball -> Brick -> (Ball, Brick, c)
collide b@(Ball (x, y) (dx, dy)) br =
  if inside b br
    then
      if ur
        then
          if dr
            then (Ball (x, y) (abs dx, dy), br & life -~ 1, 1)
            else (Ball (x, y) (dx, abs dy), br & life -~ 1, 1)
        else
          if dr
            then (Ball (x, y) (dx, - abs dy), br & life -~ 1, 1)
            else (Ball (x, y) (- abs dx, dy), br & life -~ 1, 1)
    else (b, br, 0)
  where
    (bx, by) = br ^. pos
    ur = - bh * (x - bx) - bw * (y - by) < 0
    dr = bh * (x - bx) - bw * (y - (by - bh)) > 0
