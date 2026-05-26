{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

-- TODO: is this the right module name?
module NetPBM where

import Data.ByteString (ByteString)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Byte (space, spaceChar, string)
import Text.Megaparsec.Byte.Lexer (decimal)

-- | We parse over raw bytes rather than 'Text' because the P6 raster is binary
-- data that has no faithful textual representation.
type Parser = Parsec Void ByteString

-- | Magic number at the start of the PPM file which determines the following
-- PPM format.
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

-- | Parse a single plain (ASCII) RGB triple, as used by P3.
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

-- | Parse the plain (ASCII) raster used by P3.
pRaster :: Parser Raster
pRaster = do
  many pRGB

-- | Parse a single binary sample. Samples are one byte when the maximum value
-- fits in a byte, and two bytes (big-endian) otherwise, per the PPM spec.
pSample :: Int -> Parser Int
pSample m
  | m < 256 = fromIntegral <$> anySingle
  | otherwise = do
      hi <- anySingle
      lo <- anySingle
      return (fromIntegral hi * 256 + fromIntegral lo)

-- | Parse the binary raster used by P6: exactly @count@ pixels, each three
-- samples (red, green, blue) read back to back with no separators.
pRasterBinary :: Int -> Int -> Parser Raster
pRasterBinary m pixels = count pixels pPixel
  where
    pPixel = do
      r <- pSample m
      g <- pSample m
      b <- pSample m
      return (r, g, b)

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
  r <- case n of
    -- For P3 the raster is whitespace-separated ASCII decimals.
    MagicNumberP3 -> space *> pRaster
    -- For P6 a single whitespace byte separates the header from the binary
    -- raster; we must consume exactly one so a byte that happens to be a
    -- whitespace value is not mistaken for a separator.
    MagicNumberP6 -> spaceChar *> pRasterBinary m (w * h)
  _ <- eof
  return (PPM n w h m r)
