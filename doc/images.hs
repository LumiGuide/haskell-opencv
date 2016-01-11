{-# LANGUAGE PackageImports #-}
{-# LANGUAGE DataKinds #-}

module Main where

import "base" System.Environment ( getArgs )
import "base" Control.Exception ( throw )
import "base" Data.Foldable ( forM_ )
import "base" Data.Monoid ( (<>) )
import "base" Data.Int ( Int32 )
import qualified "bytestring" Data.ByteString as B
import "linear" Linear.V2 ( V2(..) )
import "linear" Linear.V4 ( V4(..) )
import qualified "text" Data.Text as T
import qualified "thea" OpenCV as CV
import qualified "vector" Data.Vector as V

main :: IO ()
main = do
    args <- getArgs
    let destDir = case args of
                    []  -> "doc"
                    [p] -> p
                    _   -> error "Wrong number of arguments"
    render destDir arrowedLineImg "arrowedLine"
    render destDir ellipseImg     "ellipse"
    render destDir fillPolyImg    "fillPoly"
    render destDir circleImg      "circle"
    render destDir lineImg        "line"
    render destDir putTextImg     "putText"
    render destDir rectangleImg   "rectangle"

render :: FilePath ->  CV.Mat -> FilePath -> IO ()
render destDir img fp = do
    let bs = either throw id $ CV.imencode (CV.OutputPng CV.defaultPngParams) img
        dest = destDir <> "/" <> fp <> ".png"
    putStr $ "Writing file " <> dest <> " ..."
    B.writeFile dest bs
    putStrLn " OK"

transparent, black, blue, red, light_blue :: V4 Double
transparent = V4 255 255 255   0
black       = V4   0   0   0 255
blue        = V4 255   0   0 255
red         = V4   0   0 255 255
light_blue  = V4 255 100 100 255

arrowedLineImg :: CV.Mat
arrowedLineImg = CV.createMat $ do
    imgM <- CV.mkMatM (V.fromList [200, 300]) CV.MatDepth_8U 4 transparent
    CV.arrowedLine imgM (V2  10 130 :: V2 Int32) (V2 190  40 :: V2 Int32) blue 5 CV.LineType_8 0 0.15
    CV.arrowedLine imgM (V2 210  50 :: V2 Int32) (V2 250 180 :: V2 Int32) red  8 CV.LineType_8 0 0.4
    pure imgM

ellipseImg :: CV.Mat
ellipseImg = CV.createMat $ do
    imgM <- CV.mkMatM (V.fromList [200, 400]) CV.MatDepth_8U 4 transparent
    CV.ellipse imgM (V2 100 100 :: V2 Int32) (V2 90 60 :: V2 Int32)  30  0 360 blue  5  CV.LineType_AA 0
    CV.ellipse imgM (V2 300 100 :: V2 Int32) (V2 80 40 :: V2 Int32) 160 40 290 red (-1) CV.LineType_AA 0
    pure imgM

fillPolyImg :: CV.Mat
fillPolyImg = CV.createMat $ do
    imgM <- CV.mkMatM (V.fromList [h, w]) CV.MatDepth_8U 4 transparent
    CV.fillPoly imgM pts light_blue CV.LineType_AA 0
    CV.polylines imgM pts True blue 2 CV.LineType_AA 0
    pure imgM
  where
    w = 300
    h = 300
    pts = V.singleton $ V.fromList
          [ V2 (    w `div`  4) ( 7*h `div`  8)
          , V2 (  3*w `div`  4) ( 7*h `div`  8)
          , V2 (  3*w `div`  4) (13*h `div` 16)
          , V2 ( 11*w `div` 16) (13*h `div` 16)
          , V2 ( 19*w `div` 32) ( 3*h `div`  8)
          , V2 (  3*w `div`  4) ( 3*h `div`  8)
          , V2 (  3*w `div`  4) (   h `div`  8)
          , V2 ( 26*w `div` 40) (   h `div`  8)
          , V2 ( 26*w `div` 40) (   h `div`  4)
          , V2 ( 22*w `div` 40) (   h `div`  4)
          , V2 ( 22*w `div` 40) (   h `div`  8)
          , V2 ( 18*w `div` 40) (   h `div`  8)
          , V2 ( 18*w `div` 40) (   h `div`  4)
          , V2 ( 14*w `div` 40) (   h `div`  4)
          , V2 ( 14*w `div` 40) (   h `div`  8)
          , V2 (    w `div`  4) (   h `div`  8)
          , V2 (    w `div`  4) ( 3*h `div`  8)
          , V2 ( 13*w `div` 32) ( 3*h `div`  8)
          , V2 (  5*w `div` 16) (13*h `div` 16)
          , V2 (    w `div`  4) (13*h `div` 16)
          ]

circleImg :: CV.Mat
circleImg = CV.createMat $ do
    imgM <- CV.mkMatM (V.fromList [200, 400]) CV.MatDepth_8U 4 transparent
    CV.circle imgM (V2 100 100 :: V2 Int32) 90 blue  5  CV.LineType_AA 0
    CV.circle imgM (V2 300 100 :: V2 Int32) 45 red (-1) CV.LineType_AA 0
    pure imgM

lineImg :: CV.Mat
lineImg = CV.createMat $ do
    imgM <- CV.mkMatM (V.fromList [200, 300]) CV.MatDepth_8U 4 transparent
    CV.line imgM (V2  10 130 :: V2 Int32) (V2 190  40 :: V2 Int32) blue 5 CV.LineType_8 0
    CV.line imgM (V2 210  50 :: V2 Int32) (V2 250 180 :: V2 Int32) red  8 CV.LineType_8 0
    pure imgM

putTextImg :: CV.Mat
putTextImg = CV.createMat $ do
    let dims = V.fromList [50 + fromIntegral (30 * fromEnum (maxBound :: CV.FontFace)), 400]
    imgM <- CV.mkMatM dims CV.MatDepth_8U 4 transparent
    forM_ (zip [0..] [minBound .. maxBound]) $ \(n, fontFace) ->
      CV.putText imgM (T.pack $ show fontFace) (V2 10 (35 + n * 30) :: V2 Int32) fontFace 1.0 black 1 CV.LineType_AA False
    pure imgM

rectangleImg :: CV.Mat
rectangleImg = CV.createMat $ do
    imgM <- CV.mkMatM (V.fromList [200, 400]) CV.MatDepth_8U 4 transparent
    CV.rectangle imgM (CV.mkRect (V2  10 10) (V2 180 180)) blue  5  CV.LineType_8 0
    CV.rectangle imgM (CV.mkRect (V2 260 30) (V2  80 140)) red (-1) CV.LineType_8 0
    pure imgM
