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

type Ran = [Int]

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
topleft = Translate (- w / 2) (h / 2) . Pictures

initWorld :: Ran -> World
initWorld ran = World 0 0 ran ([], 1) [] Playing

draw :: World -> Picture
draw w = case w ^. status of
  Playing -> topleft $ tscore : (drawball <$> w ^. balls . _1) <> nbricks
  Aiming (x, y) -> topleft $ tscore : [drawball ((px, py), (0, 0)), Line [(px, py), (px + x, y - h / 2)]] <> nbricks
  Dead -> Color red $ Scale 0.1 0.1 $ Text ("Score: " <> show (w ^. score) <> ", stage: " <> show (w ^. stage))
  where
    nbricks = drawbrick <$> (w ^. bricks)
    tscore = Translate 5 (5 - h) $ Scale 0.1 0.1 $ Text ("s: " <> show (w ^. score) <> "   b: " <> show (w ^. balls . _2))

input :: Event -> World -> World
input (EventMotion (x, y)) w | isAiming (w ^. status) = w & status .~ Aiming (x, y)
input (EventKey (MouseButton LeftButton) Down _ (x, y)) w | isAiming (w ^. status) = w & status .~ Playing & balls . _1 .~ nballs
  where
    nballs = [((px - i * d * fst dir, py - i * d * snd dir), dir) | i <- fromIntegral <$> [0 .. (w ^. balls . _2) -1]]
    d = 4 * ballSize
    dir = normalize (x, y - h / 2 - py)
    normalize (x, y) = let l = sqrt (x * x + y * y) in (x / l, y / l)
input _ w = w

update :: Float -> World -> World
update _ w
  | null (w ^. balls . _1) && isPlaying (w ^. status) =
    if reachedBottom (w ^. bricks)
      then w & status .~ Dead
      else let (count, nbricks) = newRow n (w ^. bricks) in w & balls . _2 +~ count & bricks .~ nbricks & status .~ Aiming (0, 0) & stage .~ n
  where
    n = w ^. stage + 1
update _ w = case w ^. status of
  Playing -> w & balls . _1 .~ nballs & bricks .~ nbricks
    where
      balls' = filter insidewalls $ clampwalls . move <$> (w ^. balls . _1)
      bricks' = w ^. bricks
      (nballs, nbricks) = if null balls' then (balls', bricks') else foldl foldballs ([], bricks') balls'
      foldballs (bs, brs) b = let (nb, nbrs) = foldl foldbricks (b, []) brs in (nb : bs, nbrs)
      foldbricks (b, brs) br = let (nb, nbr) = collide b br in (nb, addbrick nbr brs)
      addbrick br@(Brick l _) brs = if l > 0 then br : brs else brs
  _ -> w

collide b@((x, y), (dx, dy)) br@(Brick life (bx, by)) =
  if inside b br
    then
      if ur
        then
          if dr
            then (((x, y), (- dx, dy)), Brick (life -1) (bx, by))
            else (((x, y), (dx, - dy)), Brick (life -1) (bx, by))
        else
          if dr
            then (((x, y), (dx, - dy)), Brick (life -1) (bx, by))
            else (((x, y), (- dx, dy)), Brick (life -1) (bx, by))
    else (b, br)
  where
    ur = - bh * (x - bx) - bw * (y - by) < 0
    dr = bh * (x - bx) - bw * (y - (by - bh)) > 0
    inside b@((x, y), (dx, dy)) br@(Brick life (bx, by)) = y > by - bh && y < by && x > bx && x < bx + bw
    s = ballSize
