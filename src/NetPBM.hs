{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module NetPBM where

import Data.Text (Text)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)

type Parser = Parsec Void Text

-- | Magic number at the start of the PPM file which determines the following
-- | PPM format.
data MagicNumber
  = MagicNumberP3
  | MagicNumberP6
  deriving (Eq, Show)

pMagicNumber :: Parser MagicNumber
pMagicNumber =
  choice
    [ MagicNumberP3 <$ string "P3",
      MagicNumberP6 <$ string "P6"
    ]

-- | Stores red, green, blue color values.
type RGB = (Int, Int, Int)

pRGB :: Parser RGB
pRGB = do
  r <- decimal <?> "red value"
  _ <- space
  g <- decimal <?> "green value"
  _ <- space
  b <- decimal <?> "blue value"
  _ <- space
  return (r, g, b)

-- | Stores RGB raster data. Since PPM stores the width and height, we can use
-- a flat list of RGB values here and deal with rows * columns later.
type Raster = [RGB]

pRaster :: Parser Raster
pRaster = do
  many pRGB

data PPM where
  PPM :: {magicNumber :: MagicNumber, width :: Int, height :: Int, maxval :: Int, raster :: Raster} -> PPM
  deriving (Eq, Show)

pPPM :: Parser PPM
pPPM = do
  n <- pMagicNumber <?> "magic number"
  _ <- space
  w <- decimal      <?> "width"
  _ <- space
  h <- decimal      <?> "height"
  _ <- space
  m <- decimal      <?> "maximum value"
  _ <- char '\n'
  r <- pRaster
  _ <- eof
  return (PPM n w h m r)
