module World where

import Data.Maybe
import Debug.Trace
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

data Brick = Brick Int Point deriving (Show)

type Ran = [Int]

type Ball = (Point, Point)

type Balls = ([Ball], Int)

type Bricks = [Brick]

data World
  = World
      { _stage :: Int,
        _score :: Int,
        _ran :: Ran,
        _balls :: Balls,
        _bricks :: Bricks,
        _aiming :: Maybe Point
      }
  | Dead
      { _stage :: Int,
        _score :: Int,
        _ran :: Ran
      }

size :: (Int, Int)
size = (400, 400)

topleft :: [Picture] -> Picture
topleft = Translate (- w) h . Pictures
  where
    w = fromIntegral $ fst size `div` 2
    h = fromIntegral $ snd size `div` 2

brickSize :: (Int, Int)
brickSize = (40, 20)

ballSize :: Float
ballSize = fromIntegral $ snd brickSize `div` 4

newBricks n = [Brick n (fromIntegral $ x * bw, 0) | x <- [0 .. s -1]]
  where
    s = fst size `div` bw
    bw = fst brickSize

initWorld :: Ran -> World
initWorld ran = World 0 0 ran ([], 3) [] Nothing

draw :: World -> Picture
draw (World n s ran (balls, nb) bricks aiming) = topleft (score : nballs <> nbricks)
  where
    nbricks = drawbrick <$> bricks
    nballs = case aiming of
      Just (x, y) -> [drawball ((px, py), (0, 0)), Line [(px, py), (px + x, y - h / 2)]]
      Nothing -> drawball <$> balls
      where
        px = fromIntegral $ fst size `div` 2
        py = 2 * ballSize - h
        h = fromIntegral (snd size)
    score = Translate 5 (5 - sh) $ Scale 0.1 0.1 $ Text ("s: " <> show s <> "   b: " <> show nb)
    sw2 = fromIntegral $ fst size `div` 2
    sh = fromIntegral $ snd size
    drawbrick (Brick life (x', y')) = Pictures [Color blue $ Polygon [(x, y), (x + w, y), (x + w, y - h), (x, y - h)], Color white $ Translate (x + 2) (y -12) $ Scale 0.1 0.1 $ Text (show life)]
      where
        x = x' + 1
        y = y' - 1
    drawball ((x, y), _) = Color green $ Translate x y $ Circle ballSize
    w = fromIntegral $ fst brickSize - 2
    h = fromIntegral $ snd brickSize - 2
draw (Dead n s r) = Color red $ Scale 0.1 0.1 $ Text ("Score: " <> show s <> ", stage: " <> show n)

input :: Event -> World -> World
input (EventMotion (x, y)) w | isJust (_aiming w) = w {_aiming = Just (x, y)}
input (EventKey (MouseButton LeftButton) Down _ (x, y)) (World n s ran (_, nb) bricks aiming) | isJust aiming = World n s ran (balls, nb) bricks Nothing
  where
    balls = [((px - i * d * fst dir, py - i * d * snd dir), dir) | i <- fromIntegral <$> [0 .. nb -1]]
    d = 4 * ballSize
    px = fromIntegral $ fst size `div` 2
    py = 2 * ballSize - h
    h = fromIntegral (snd size)
    dir' = (x, y - h / 2 - py)
    ld = sqrt (fst dir' ** 2 + snd dir' ** 2)
    dir = (fst dir' / ld, snd dir' / ld)
input _ w = w

update :: Float -> World -> World
update _ (World n' s ran ([], nb) bricks aiming)
  | isNothing aiming =
    if reachedBottom bricks
      then Dead n' s ran
      else World n s ran ([], nb) (newRow n bricks) (Just (0, 0))
  where
    n = n' + 1
    reachedBottom [] = False
    reachedBottom ((Brick _ (_, y)) : _) = - y >= fromIntegral (snd size - snd brickSize)
    newRow n bricks = filter alive (down <$> bricks) <> newBricks n
    down (Brick l (x, y)) = Brick l (x, y - h)
    alive (Brick l _) = l > 0
    h = fromIntegral $ snd brickSize
update _ (World n s ran (balls, nb) bricks aiming) = World n s ran (nballs, nb) nbricks aiming
  where
    balls' = filter inside $ walls . move <$> balls
    inside ((px, py), (_, dy)) = px >= 0 && px < w && (py <= 0 && py > - h || dy > 0)
    walls ((px, py), (dx, dy)) = ((npx, npy), (ndx, ndy))
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
    move ((px, py), (dx, dy)) = ((px + v * dx, py + v * dy), (dx, dy))
    v = 5
    (nballs, nbricks) = (balls', bricks)
update _ (Dead n s r) = Dead n s r
