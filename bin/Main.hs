{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Function (on)
import Data.List (foldl')
import qualified Data.Text.IO as TIO
import Graphics.Gloss
import NetPBM
import Text.Megaparsec

-- | Make a component clamped to the range [0, 1] from a color code (clamped
-- from [0, 255] and the maximum value.
makeComponent :: Int -> Int -> Float
makeComponent = (/) `on` fromIntegral

-- | A 1x1 pixel at point (x, y) and color c.
pixel :: Int -> Int -> Color -> Picture
pixel x y c = translate (fromIntegral x) (fromIntegral y) $ color c $ rectangleSolid 10 10

-- | Create a Picture given a raster, width, and maximum value.
render ::
  Raster ->
  Int ->
  Int ->
  Picture
render raster width maxval = translate (-150) 150 $ foldl' render' Blank $ zip [0 ..] raster
  where
    makeColors (r, g, b) = makeColor (makeComponent r maxval) (makeComponent g maxval) (makeComponent b maxval) 1

    render' ::
      Picture ->
      (Int, (Int, Int, Int)) ->
      Picture
    render' pic (index, (r, g, b)) =
      let x = (index `mod` width) * 10
          y = (index `div` width) * (-10)
          makePixel = pixel x y . makeColors
       in pictures [pic, makePixel (r, g, b)]

main :: IO ()
main = do
  content <- TIO.readFile "./examples/P3/smiley-20x.ppm"
  case runParser pPPM "smiley.ppm" content of
    Left err -> Prelude.putStrLn $ errorBundlePretty err
    Right PPM {raster, width, maxval} -> do
      display (InWindow "netpbm" (400, 400) (10, 10)) white $ render raster width maxval
