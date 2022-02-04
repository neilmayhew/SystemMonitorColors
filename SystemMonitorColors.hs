{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

import Control.Monad
import Data.Foldable
import Data.List (intercalate)
import Data.Word
import Graphics.Color.Model
import Graphics.Pixel
import Numeric
import Options.Applicative
import System.Environment
import System.Exit
import Test.Hspec
import Text.Printf

import qualified System.Console.Terminal.Size as TS

data Options = Options
  { optSimple :: Bool
  , optUnit :: Bool
  , optAnalyze :: Bool
  , optDconf :: Bool
  , optHtml :: Bool
  } deriving (Show)

main :: IO ()
main = do

  cols <- maybe 100 TS.width <$> TS.size

  Options {..} <- customExecParser
    ( prefs $ columns cols )
    ( info
      ( helper <*> do
          optSimple <- switch $
            short 's' <> long "simple" <>
            help "Run simple tests"
          optUnit <- switch $
            short 'u' <> long "unit" <>
            help "Run unit tests"
          optAnalyze <- switch $
            short 'a' <> long "analyze" <>
            help "Run an analysis of existing and default colors"
          optDconf <- switch $
            short 'd' <> long "dconf" <>
            help "Output a cpu-colors value for use with dconf"
          optHtml <- switch $
            short 'm' <> long "html" <>
            help "Output an HTML file for previewing the colors"
          pure Options{..}
      )
      ( fullDesc <> header "Generate CPU colors for use in Gnome System Monitor" )
    )

  when optSimple $ do
    let c = ColorHSV 0 0.5 (0.5 :: Double)
    print c
    print $ toComponents c
    let r = hsv2rgb c
    print r
    print $ toComponents r
    print $ toPixel8 (Pixel r)
    putStrLn . toCSShex $ toPixel8 (Pixel r)

  when optUnit $ do
    withArgs [] $ hspec tests

  when optAnalyze $ do
    let fromPixel8 = fmap toDouble
        analyze css = do
          let rgb' = fromCSShex css
              hsv' = liftPixel rgb2hsv $ fromPixel8 rgb'
              (h, _, _) = toComponents $ pixelColor hsv'
          printf "%s -> %s -> %s (%5.2f)\n" css (show rgb') (show hsv') (h * 16)
    putStrLn "Default"
    traverse_ analyze defaultColors
    putStrLn "Existing"
    traverse_ analyze existingColors

  let interpolate t a b = a * (1 - t) + b * t
      colors =
        [ hsv
          ( interpolate (n / 15) 0.35 1.15 * 360
          , interpolate (n / 15) 0.9 0.6
          , interpolate (n / 15) 0.7 1.0
          )
          | n <- [0 .. 15 :: Double]
        ]

  when optDconf $ do
    printf "@a(us) [%s]" $ intercalate ", " $ zipWith (printf "(%d, '%s')") [0::Int ..] colors

  when optHtml $
    putStr . unlines $ htmlHeader <> concatMap htmlRow (reverse colors) <> htmlFooter

htmlHeader :: [String]
htmlHeader =
  [ "<html>"
  , "<style>"
  , "  body {"
  , "    font-family: sans-serif;"
  , "  }"
  , "  table {"
  , "     width: 40em;"
  , "     margin: auto;"
  , "     border-collapse: collapse;"
  , "  }"
  , "  td {"
  , "    text-align: center;"
  , "    color: gray;"
  , "  }"
  , "</style>"
  , "  <body>"
  , "    <table>"
  ]

htmlRow :: String -> [String]
htmlRow c =
  [ "      <tr>"
  , "        <td style=\"background-color: " <> c <> "\">"
  , "          " <> c
  , "        </td>"
  , "      </tr"
  ]

htmlFooter :: [String]
htmlFooter =
  [ "    </table>"
  , "  </body>"
  , "</html>"
  ]

defaultColors :: [String]
defaultColors =
  [ "#e6194b"
  , "#f58231"
  , "#ffe119"
  , "#bfef45"
  , "#3cb44b"
  , "#42d4f4"
  , "#4363d8"
  , "#911eb4"
  , "#f032e6"
  , "#fabebe"
  , "#ffd8b1"
  , "#fffac8"
  , "#aaffc3"
  , "#469990"
  , "#000075"
  , "#e6beff"
  ]

existingColors :: [String]
existingColors =
  [ "#FF6E00"
  , "#CB0C29"
  , "#49A835"
  , "#2D7DB3"
  ]

fromCSShex :: String -> Pixel RGB Word8
fromCSShex s = PixelRGB (hx r) (hx g) (hx b) where
  rgb' = drop 1 s
  (r, gb) = splitAt 2 rgb'
  (g, b)  = splitAt 2 gb
  hx = fst . head . readHex

toCSShex :: Pixel RGB Word8 -> String
toCSShex (PixelRGB r g b) = printf "#%02X%02X%02X" r g b

toCSSrgb :: Pixel RGB Word8 -> String
toCSSrgb (PixelRGB r g b) = printf "rgb(%d,%d,%d)" r g b

rgb :: (Double, Double, Double) -> String
rgb (r, g, b) = toCSSrgb . toPixel8 $ PixelRGB r g b

hsv :: (Double, Double, Double) -> String
hsv (h, s, v) = toCSSrgb . toPixel8 . liftPixel hsv2rgb $ PixelHSV (fractional $ h / 360) s v

rgbhex :: (Double, Double, Double) -> String
rgbhex (r, g, b) = toCSShex . toPixel8 $ PixelRGB r g b

hsvhex :: (Double, Double, Double) -> String
hsvhex (h, s, v) = toCSShex . toPixel8 . liftPixel hsv2rgb $ PixelHSV (fractional $ h / 360) s v

fractional :: RealFrac a => a -> a
fractional = snd . properFraction @_ @Int

tests :: Spec
tests =
  context "Colors" $ do
    specify "fromCSShex" $ do
      fromCSShex "#566973" `shouldBe` PixelRGB 0x56 0x69 0x73
      fromCSShex "#FFFF80" `shouldBe` PixelRGB 0xFF 0xFF 0x80
      fromCSShex "#EBF8FF" `shouldBe` PixelRGB 0xEB 0xF8 0xFF
      fromCSShex "#175473" `shouldBe` PixelRGB 0x17 0x54 0x73
    specify "toCSShex" $ do
      toCSShex (PixelRGB 0x56 0x69 0x73) `shouldBe` "#566973"
      toCSShex (PixelRGB 0xFF 0xFF 0x80) `shouldBe` "#FFFF80"
      toCSShex (PixelRGB 0xEB 0xF8 0xFF) `shouldBe` "#EBF8FF"
      toCSShex (PixelRGB 0x17 0x54 0x73) `shouldBe` "#175473"
    specify "toCSSrgb" $ do
      toCSSrgb (PixelRGB 0x56 0x69 0x73) `shouldBe` "rgb(86,105,115)"
      toCSSrgb (PixelRGB 0xFF 0xFF 0x80) `shouldBe` "rgb(255,255,128)"
      toCSSrgb (PixelRGB 0xEB 0xF8 0xFF) `shouldBe` "rgb(235,248,255)"
      toCSSrgb (PixelRGB 0x17 0x54 0x73) `shouldBe` "rgb(23,84,115)"
    specify "rgb" $ do
      rgb (0x56/255, 0x69/255, 0x73/255) `shouldBe` "rgb(86,105,115)"
      rgb (0xFF/255, 0xFF/255, 0x80/255) `shouldBe` "rgb(255,255,128)"
      rgb (0xEB/255, 0xF8/255, 0xFF/255) `shouldBe` "rgb(235,248,255)"
      rgb (0x17/255, 0x54/255, 0x73/255) `shouldBe` "rgb(23,84,115)"
    specify "hsv" $ do
      hsv (200, 0.25, 0.45) `shouldBe` "rgb(86,105,115)"
      hsv ( 60, 0.50, 1.00) `shouldBe` "rgb(255,255,128)"
      hsv (200, 0.08, 1.00) `shouldBe` "rgb(235,248,255)"
      hsv (200, 0.80, 0.45) `shouldBe` "rgb(23,84,115)"
    specify "rgbhex" $ do
      rgbhex (0x56/255, 0x69/255, 0x73/255) `shouldBe` "#566973"
      rgbhex (0xFF/255, 0xFF/255, 0x80/255) `shouldBe` "#FFFF80"
      rgbhex (0xEB/255, 0xF8/255, 0xFF/255) `shouldBe` "#EBF8FF"
      rgbhex (0x17/255, 0x54/255, 0x73/255) `shouldBe` "#175473"
    specify "hsvhex" $ do
      hsvhex (200, 0.25, 0.45) `shouldBe` "#566973"
      hsvhex ( 60, 0.50, 1.00) `shouldBe` "#FFFF80"
      hsvhex (200, 0.08, 1.00) `shouldBe` "#EBF8FF"
      hsvhex (200, 0.80, 0.45) `shouldBe` "#175473"
