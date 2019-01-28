module Main where

import Prelude 
import Partial.Unsafe (unsafePartial)
import Effect (Effect)
import Effect.Console (log)
import Data.Maybe (Maybe(..), maybe)
import Data.Int (toNumber, floor)
import Data.Array
import Data.Traversable
import Math as Math
import Web.HTML (window)
import Web.HTML.Window (innerWidth, innerHeight)
import Data.Unfoldable
import Data.Tuple
import P5 
import P5.Rendering
import P5.Color
import P5.Shape
import P5.Structure

type AppState = {
  p5 :: P5
}

enumFromThenTo :: Int -> Int -> Int -> Array Int
enumFromThenTo a b c = do
  unfoldr (go (b - a) c) a
  where
    go step to e
      | e <= to = Just (Tuple e (e + step))
      | otherwise = Nothing

initialState :: Maybe AppState
initialState = Nothing

perlin1D :: Number -> Int -> Number
perlin1D x octaves = do
  let permutation = 
        [ 139, 30, 126, 141, 43, 53, 57, 8, 158, 84, 157, 251, 239, 95, 247, 223, 247, 90, 202, 0, 150, 42, 60, 208, 226, 48, 232, 38, 148, 114, 140, 178, 64, 101, 26, 10, 88, 36, 107, 139, 60, 252, 127, 226, 109, 179, 144, 120, 192, 2, 160, 87, 70, 11, 112, 194, 95, 102, 219, 164, 52, 173, 148, 112, 147, 249, 136, 207, 204, 248, 144, 14, 149, 189, 61, 36, 29, 122, 29, 108, 238, 134, 237, 239, 236, 36, 139, 20, 172, 240, 37, 100, 135, 72, 232, 122, 18, 202, 116, 77, 59, 7, 41, 81, 215, 128, 232, 65, 172, 92, 249, 224, 238, 55, 9, 216, 115, 57, 157, 52, 112, 161, 137, 110, 195, 100, 255, 22, 52, 173, 1, 156, 162, 67, 189, 49, 210, 68, 55, 254, 253, 105, 103, 35, 204, 57, 10, 160, 100, 227, 178, 145, 177, 49, 236, 117, 228, 144, 31, 204, 33, 92, 227, 128, 107, 236, 40, 208, 60, 247, 17, 174, 29, 24, 207, 224, 33, 24, 212, 213, 195, 97, 53, 177, 221, 86, 4, 226, 160, 113, 132, 104, 18, 31, 189, 82, 78, 240, 252, 19, 41, 94, 87, 46, 247, 174, 195, 30, 159, 184, 200, 92, 41, 240, 154, 95, 99, 100, 101, 201, 27, 228, 215, 225, 122, 214, 190, 165, 72, 182, 118, 216, 240, 110, 28, 129, 97, 85, 27, 35, 94, 191, 54, 94, 193, 178, 146, 238, 124, 64, 17, 68, 235, 179, 88, 91 ]
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
    pure unit

  draw p do
    background3 p palette.b Nothing
    stroke p palette.a
    strokeWeight p 1.0
    traverse_ (\x -> do
      ellipse p 
        (toNumber x) 
        (h / 2.0 
        + 100.0 
        * (0.5 - perlin1D (19.0 * (toNumber x / Math.floor w)) 0))
        4.0 Nothing
    ) $ enumFromThenTo 1 8 (floor w)
    pure unit

  case mAppState of
    (Just _) -> do
      clear p
      redraw p Nothing
    _ -> pure unit

  pure $ Just { p5: p }
