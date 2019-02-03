module Main where

import Prelude 
import Debug.Trace (trace)
import Partial.Unsafe (unsafePartial)
import Effect (Effect)
import Effect.Console (log, logShow)
import Data.Maybe (Maybe(..), maybe)
import Data.Int (toNumber, floor)
import Data.Array
import Control.Safely
import Data.Traversable as UnsafeTraversable
import Data.Foldable (sum)
import Data.Unfoldable
import Math as Math
import Web.HTML (window)
import Web.HTML.Window (innerWidth, innerHeight)
import Data.Tuple
import Control.Monad.State
import P5 
import P5.Rendering
import P5.Color
import P5.Shape
import P5.Structure
import P5.Math

type AppState = {
  p5 :: P5
}

permutation :: Array Int
permutation = 
  [ 
  151,160,137,91,90,15,
   131,13,201,95,96,53,194,233,7,225,140,36,103,30,69,142,8,99,37,240,21,10,23,
   190, 6,148,247,120,234,75,0,26,197,62,94,252,219,203,117,35,11,32,57,177,33,
   88,237,149,56,87,174,20,125,136,171,168, 68,175,74,165,71,134,139,48,27,166,
   77,146,158,231,83,111,229,122,60,211,133,230,220,105,92,41,55,46,245,40,244,
   102,143,54, 65,25,63,161, 1,216,80,73,209,76,132,187,208, 89,18,169,200,196,
   135,130,116,188,159,86,164,100,109,198,173,186, 3,64,52,217,226,250,124,123,
   5,202,38,147,118,126,255,82,85,212,207,206,59,227,47,16,58,17,182,189,28,42,
   223,183,170,213,119,248,152, 2,44,154,163, 70,221,153,101,155,167, 43,172,9,
   129,22,39,253, 19,98,108,110,79,113,224,232,178,185, 112,104,218,246,97,228,
   251,34,242,193,238,210,144,12,191,179,162,241, 81,51,145,235,249,14,239,107,
   49,192,214, 31,181,199,106,157,184, 84,204,176,115,121,50,45,127, 4,150,254,
   138,236,205,93,222,114,67,29,24,72,243,141,128,195,78,66,215,61,156,180
   ,151,160,137,91,90,15,
   131,13,201,95,96,53,194,233,7,225,140,36,103,30,69,142,8,99,37,240,21,10,23,
   190, 6,148,247,120,234,75,0,26,197,62,94,252,219,203,117,35,11,32,57,177,33,
   88,237,149,56,87,174,20,125,136,171,168, 68,175,74,165,71,134,139,48,27,166,
   77,146,158,231,83,111,229,122,60,211,133,230,220,105,92,41,55,46,245,40,244,
   102,143,54, 65,25,63,161, 1,216,80,73,209,76,132,187,208, 89,18,169,200,196,
   135,130,116,188,159,86,164,100,109,198,173,186, 3,64,52,217,226,250,124,123,
   5,202,38,147,118,126,255,82,85,212,207,206,59,227,47,16,58,17,182,189,28,42,
   223,183,170,213,119,248,152, 2,44,154,163, 70,221,153,101,155,167, 43,172,9,
   129,22,39,253, 19,98,108,110,79,113,224,232,178,185, 112,104,218,246,97,228,
   251,34,242,193,238,210,144,12,191,179,162,241, 81,51,145,235,249,14,239,107,
   49,192,214, 31,181,199,106,157,184, 84,204,176,115,121,50,45,127, 4,150,254,
   138,236,205,93,222,114,67,29,24,72,243,141,128,195,78,66,215,61,156,180]

enumFromThenTo :: Int -> Int -> Int -> Array Int
enumFromThenTo a b c = do
  unfoldr (go (b - a) c) a
  where
    go step to e
      | e <= to = Just (Tuple e (e + step))
      | otherwise = Nothing

initialState :: Maybe AppState
initialState = Nothing

octavePerlin1D :: Number -> Int -> Number -> Number
octavePerlin1D x octave persistence = evalState octavePerlin1D' 0.0
  where 
    octavePerlin1D' :: State Number Number
    octavePerlin1D' = do 
      total <- sum <$> UnsafeTraversable.traverse (\i -> do
        let period = 2.0 `Math.pow` (toNumber i)
            amplitude = persistence `Math.pow` (toNumber i)
        maxValue <- get
        put (maxValue + amplitude)
        pure $ perlin1D (x * period) * amplitude
      ) (0..octave)
      maxValue <- get
      pure $ total / maxValue

octavePerlin2D :: Number -> Number -> Int -> Number -> Number
octavePerlin2D x y octave persistence = evalState octavePerlin2D' 0.0
  where 
    octavePerlin2D' :: State Number Number
    octavePerlin2D' = do 
      total <- sum <$> UnsafeTraversable.traverse (\i -> do
        let period = 2.0 `Math.pow` (toNumber i)
            amplitude = persistence `Math.pow` (toNumber i)
        maxValue <- get
        put (maxValue + amplitude)
        pure $ perlin2D (x * period) (y * period) * amplitude
      ) (0..octave)
      maxValue <- get
      pure $ total / maxValue

perlin1D :: Number -> Number
perlin1D x = do
  let
    a = (toNumber 
      $ unsafePartial
      $ permutation `unsafeIndex` (floor x `mod` 256))
      / 255.0
    b = (toNumber 
      $ unsafePartial
      $ permutation `unsafeIndex` (floor (x + 1.0) `mod` 256))
      / 255.0
    sx = x - (Math.floor x)
  lerp (sCurve sx) a b
  where
    sCurve :: Number -> Number
    sCurve t = t * t * (3.0 - 2.0 * t)
    lerp :: Number -> Number -> Number -> Number
    lerp t a b = a + t * (b - a)

perlin2D :: Number -> Number -> Number
perlin2D x y = do
  let 
    inc :: Int -> Int
    inc x = (x + 1)
    xi = floor x `mod` 256
    yi = floor y `mod` 256
    xf = x - (Math.floor x)
    yf = y - (Math.floor y)
    aa = toNumber
      $ unsafePartial
      $ permutation `unsafeIndex`
      ((permutation `unsafeIndex` xi) + yi)
    ab = toNumber
      $ unsafePartial
      $ permutation `unsafeIndex`
      ((permutation `unsafeIndex` (inc xi)) + yi)
    ba = toNumber
      $ unsafePartial
      $ permutation `unsafeIndex`
      ((permutation `unsafeIndex` xi) + (inc yi))
    bb = toNumber
      $ unsafePartial
      $ permutation `unsafeIndex`
      ((permutation `unsafeIndex` (inc xi)) + (inc yi))
    sx = sCurve xf
    sy = sCurve yf
    x1 = lerp
      sx
      (grad aa xf yf)
      (grad ab (xf - 1.0) yf)
    x2 = lerp
      sx
      (grad ba xf (yf - 1.0))
      (grad bb (xf - 1.0) (yf - 1.0))
  ((lerp sy x1 x2) + 1.0) / 2.0
  where
    sCurve :: Number -> Number
    sCurve t = t * t * t * (t * (t * 6.0 - 15.0) + 10.0)
    lerp :: Number -> Number -> Number -> Number
    lerp t a b = a + t * (b - a)
    grad :: Number -> Number -> Number -> Number
    grad hash x y
      | hash < 64.0 = x
      | hash < 128.0 = y
      | hash < 192.0 = -x
      | otherwise = -y

drawPerlin3d :: 
  P5 -> Int -> Int -> Int -> Number -> Number -> Number -> Effect Unit
drawPerlin3d p x y octaves persistence w h = do
  let 
    r = 2
    grid = do 
      x' <- enumFromThenTo x (x + r) (x + floor w - r) 
      y' <- enumFromThenTo y (y + r) (y + floor h - r)
      pure $ Tuple x' y'
  traverse_ (\(Tuple x y) -> do
    let e = octavePerlin2D
              (9.0 * (toNumber x / Math.floor w)) 
              (9.0 * (toNumber y / Math.floor h))
              octaves
              persistence
    fill4 p (255.0) (Just (255.0 * e))
    rect p (toNumber x) 
      (toNumber y) (toNumber r) (toNumber r) Nothing Nothing
    pure unit
  ) grid

  pure unit

drawPerlin2d :: P5 -> Number -> Number -> Effect Unit
drawPerlin2d p w h = do
  background3 p "gray" Nothing
  stroke p "#4d0c40"
  strokeWeight p 1.0
  traverse_ (\x -> do
    ellipse p 
      (toNumber x) 
      (h / 4.0 
      + 100.0 
      * (0.5 - 
          perlin1D 
            (9.0 * (toNumber x / Math.floor w))))
      4.0 Nothing
    ellipse p 
      (toNumber x) 
      (h / 2.0 
      + 100.0 
      * (0.5 - 
          (octavePerlin1D 
            (9.0 * (toNumber x / Math.floor w)) 2 0.6)))
      4.0 Nothing
    ellipse p 
      (toNumber x) 
      ((3.0 * h) / 4.0 
      + 100.0 
      * (0.5 - 
          (octavePerlin1D 
            (9.0 * (toNumber x / Math.floor w)) 3 0.6)))
      4.0 Nothing
  ) $ enumFromThenTo 1 2 (floor w)
  pure unit

main :: Maybe AppState -> Effect (Maybe AppState)
main mAppState = do
  win <- window
  w <- toNumber <$> innerWidth win
  h <- toNumber <$> innerHeight win
  p <- maybe getP5 (\x -> pure x.p5) mAppState

  let palette = 
        { a: "#4d0c40"
        , b: "gray"
        , c: "#b29179"
        , d: "#c0a476"
        , e: "#9d7f38"
        }
  setup p do
    _ <- createCanvas p w h Nothing
    noLoop p
    pure unit

  draw p do
    background3 p "black" Nothing
    noStroke p
    drawPerlin3d p 1 1 0 0.5 (w / 2.0) (h / 2.0)
    drawPerlin3d 
      p 
      (floor (w / 2.0)) 
      1
      1 0.5
      (w / 2.0) (h / 2.0)
    drawPerlin3d 
      p 
      (floor (w / 2.0)) 
      (floor (h / 2.0)) 
      2 0.5
      (w / 2.0) (h / 2.0)
    drawPerlin3d 
      p 
      1
      (floor (h / 2.0)) 
      5 0.5
      (w / 2.0) (h / 2.0)

  case mAppState of
    (Just _) -> do
      clear p
      redraw p Nothing
    _ -> pure unit

  pure $ Just { p5: p }
