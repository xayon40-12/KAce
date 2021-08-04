{-# LANGUAGE TemplateHaskell #-}

module World where

import Ball
import Brick
import Constants
import Data.Maybe
import Debug.Trace
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Lens.Micro
import Lens.Micro.TH

data Status = Aiming Point | Playing | Dead

data World = World
  { _stage :: Int,
    _score :: Int,
    _ran :: Ran,
    _balls :: Balls,
    _bricks :: Bricks,
    _status :: Status
  }

makeLenses ''World

isAiming (Aiming _) = True
isAiming _ = False

isPlaying Playing = True
isPlaying _ = False

topleft :: [Picture] -> Picture
topleft = Translate (- sw / 2) (sh / 2) . Pictures

initWorld :: Ran -> World
initWorld ran = World 1 0 ran1 ([], 1) nbricks (Aiming out)
  where
    (ran1, nbricks) = newBricks 1 ran

draw :: World -> Picture
draw w = case w ^. status of
  Playing -> topleft $ tscore : lim : (drawball <$> w ^. balls . _1) <> nbricks
  Aiming (x, y) -> topleft $ tscore : lim : nbricks <> inside (x, y)
  Dead -> Color red $ Scale 0.1 0.1 $ Text ("Score: " <> show (w ^. score) <> ", stage: " <> show (w ^. stage))
  where
    inside (x, y) = drawball ((px, py), (0, 0)) : [Line [(px, py), (px + x, y - sh / 2)] | x >= - sw / 2 && x <= sw / 2 && y <= sh / 2 && y >= bh - sh / 2]
    nbricks = drawbrick <$> (w ^. bricks)
    tscore = Translate 5 (5 - sh) $ Scale 0.1 0.1 $ Text ("s: " <> show (w ^. score) <> "   b: " <> show (w ^. balls . _2))
    lim = Color red $ Line [(0, bh - sh), (sw, bh - sh)]

input :: Event -> World -> World
input (EventMotion (x, y)) w | isAiming (w ^. status) = w & status .~ Aiming (x, y)
input (EventKey (MouseButton LeftButton) Down _ (x, y)) w = case w ^. status of
  Aiming (x, y) | y > bh - sh / 2 -> w & status .~ Playing & balls . _1 .~ nballs
  _ -> w
  where
    nballs = [((px - i * d * fst dir, py - i * d * snd dir), dir) | i <- fromIntegral <$> [0 .. (w ^. balls . _2) -1]]
    d = 0.25 * ballSize
    dir = normalize (x, y - sh / 2 - py)
    normalize (x, y) = let l = sqrt (x * x + y * y) in (x / l, y / l)
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
  Playing -> w & balls . _1 .~ nballs & bricks .~ nbricks & balls . _2 +~ count & score +~ del
    where
      balls' = filter insidewalls $ clampwalls . move <$> (w ^. balls . _1)
      bricks' = w ^. bricks
      (nballs, del, (nbricks, count)) = if null balls' then (balls', 0, (bricks', 0)) else foldl foldballs ([], 0, (bricks', 0)) balls'
      foldballs (bs, del, (brs, c)) b = let (nb, del', nbrs) = foldl foldbricks (b, del, ([], c)) brs in (nb : bs, del', nbrs)
      foldbricks (b, del, brs) br = let (nb, nbr, del') = collide b br in (nb, del + del', addbrick nbr brs)
      addbrick br (brs, c) = if alive br then (br : brs, c) else (brs, c + 1)
  _ -> w

collide b@((x, y), (dx, dy)) br =
  if inside b br
    then
      if ur
        then
          if dr
            then (((x, y), (abs dx, dy)), br & life -~ 1, 1)
            else (((x, y), (dx, abs dy)), br & life -~ 1, 1)
        else
          if dr
            then (((x, y), (dx, - abs dy)), br & life -~ 1, 1)
            else (((x, y), (- abs dx, dy)), br & life -~ 1, 1)
    else (b, br, 0)
  where
    (bx, by) = br ^. pos
    ur = - bh * (x - bx) - bw * (y - by) < 0
    dr = bh * (x - bx) - bw * (y - (by - bh)) > 0
    inside b@((x, y), (dx, dy)) br@(Brick _ (bx, by) _) = y >= by - bh && y <= by && x >= bx && x <= bx + bw
    s = ballSize
