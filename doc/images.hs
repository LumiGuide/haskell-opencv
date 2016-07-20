{-# language TemplateHaskell #-}
{-# language OverloadedStrings #-}
{-# language FlexibleContexts #-}

module Main where

import "base" Data.Functor ( void )
import "base" Data.Foldable ( forM_ )
import "base" Data.Int
import "base" Data.Monoid ( (<>) )
import "base" Data.Proxy
import "base" Data.Word
import "base" GHC.TypeLits
import "base" System.IO.Unsafe ( unsafePerformIO )
import qualified "bytestring" Data.ByteString as B
import "linear" Linear.Vector ( (^+^) )
import "linear" Linear.V2 ( V2(..) )
import "linear" Linear.V4 ( V4(..) )
import "opencv" OpenCV
import "opencv" OpenCV.Unsafe
import "primitive" Control.Monad.Primitive ( PrimMonad, PrimState )
import qualified "text" Data.Text as T
import qualified "vector" Data.Vector as V
import "transformers" Control.Monad.Trans.Class ( lift )

import "this" ExampleExtractor ( render, extractExampleImages )

--------------------------------------------------------------------------------

transparent, white, black, blue, red :: Scalar
transparent = toScalar (V4 255 255 255   0 :: V4 Double)
white       = toScalar (V4 255 255 255 255 :: V4 Double)
black       = toScalar (V4   0   0   0 255 :: V4 Double)
blue        = toScalar (V4 255   0   0 255 :: V4 Double)
red         = toScalar (V4   0   0 255 255 :: V4 Double)

type Birds_768x512    = Mat (ShapeT [512, 768]) ('S 3) ('S Word8)
type Flower_768x512   = Mat (ShapeT [512, 768]) ('S 3) ('S Word8)
type Sailboat_768x512 = Mat (ShapeT [512, 768]) ('S 3) ('S Word8)
type Birds_512x341    = Mat (ShapeT [341, 512]) ('S 3) ('S Word8)
type Flower_512x341   = Mat (ShapeT [341, 512]) ('S 3) ('S Word8)
type Sailboat_512x341 = Mat (ShapeT [341, 512]) ('S 3) ('S Word8)
type Frog             = Mat (ShapeT [390, 500]) ('S 3) ('S Word8)
type Lambda           = Mat (ShapeT [256, 256]) ('S 1) ('S Word8)
type Circles_1000x625 = Mat (ShapeT [625, 1000]) ('S 3) ('S Word8)
type Building_868x600 = Mat (ShapeT [600, 868]) ('S 3) ('S Word8)

birds_768x512 :: Birds_768x512
birds_768x512 = exceptError $ coerceMat $ unsafePerformIO $
                  imdecode ImreadUnchanged <$> B.readFile "data/kodim23.png"

flower_768x512 :: Flower_768x512
flower_768x512 =
    exceptError $ coerceMat $ unsafePerformIO $
      imdecode ImreadUnchanged <$> B.readFile "data/kodim07.png"

sailboat_768x512 :: Sailboat_768x512
sailboat_768x512 =
    exceptError $ coerceMat $ unsafePerformIO $
      imdecode ImreadUnchanged <$> B.readFile "data/kodim06.png"

smallerKodakImg
    :: Mat (ShapeT [512, 768]) ('S 3) ('S Word8)
    -> Mat (ShapeT [341, 512]) ('S 3) ('S Word8)
smallerKodakImg img =
    exceptError $ coerceMat =<<
      resize (ResizeAbs $ toSize2i (V2 512 341 :: V2 Int32))
             InterArea
             img

birds_512x341 :: Birds_512x341
birds_512x341 = smallerKodakImg birds_768x512

flower_512x341 :: Flower_512x341
flower_512x341 = smallerKodakImg flower_768x512

sailboat_512x341 :: Flower_512x341
sailboat_512x341 = smallerKodakImg sailboat_768x512

frog :: Frog
frog =
    exceptError $ coerceMat $ unsafePerformIO $
      imdecode ImreadUnchanged <$> B.readFile "data/kikker.jpg"

lambda :: Lambda
lambda =
    exceptError $ coerceMat $ unsafePerformIO $
      imdecode ImreadUnchanged <$> B.readFile "data/lambda.png"

circles_1000x625 :: Circles_1000x625
circles_1000x625 =
    exceptError $ coerceMat $ unsafePerformIO $
      imdecode ImreadUnchanged <$> B.readFile "data/circles.png"

building_868x600 :: Building_868x600
building_868x600 =
    exceptError $ coerceMat $ unsafePerformIO $
      imdecode ImreadUnchanged <$> B.readFile "data/building.jpg"

--------------------------------------------------------------------------------

-- We use some padding around the small image in which we draw the
-- lines. This is because antialiasing doesn't seem to work near the
-- edges of an image.
lineTypeImg
    :: forall (h :: Nat) (w :: Nat) (p :: Nat)
     . ( h ~ 5
       , w ~ (h * 3)
       , p ~ 20
       )
    => LineType
    -> Mat ('S ['D, 'D]) ('S 4) ('S Word8)
lineTypeImg lineType = exceptError $ do
    img <- withMatM (h + 2 * p ::: w + 2 * p ::: Z)
                    (Proxy :: Proxy 4)
                    (Proxy :: Proxy Word8)
                    transparent $ \imgM -> do
             lift $ line imgM (pure p + V2 0 h) (pure p + V2 w 0) black 1 lineType 0
    resize (ResizeRel $ pure zoom) InterNearest
           =<< matSubRect img (mkRect (pure p) (V2 w h))
  where
    w, h, p :: Int32
    w = fromInteger $ natVal (Proxy :: Proxy w)
    h = fromInteger $ natVal (Proxy :: Proxy h)
    p = fromInteger $ natVal (Proxy :: Proxy p)
    zoom = 8

fontImg
    :: Font
    -> Mat ('S ['D, 'D]) ('S 4) ('S Word8)
fontImg font = exceptError $
    withMatM (th * 3 ::: tw ::: Z)
             (Proxy :: Proxy 4)
             (Proxy :: Proxy Word8)
             transparent $ \imgM -> do
      putText
        imgM
        txt
        (V2 0 (th * 2 - baseLine) :: V2 Int32)
        font
        black
        thickness
        LineType_AA False
  where
    txt = "The quick brown fox jumps over the lazy dog"
    (size2i, baseLine) = getTextSize txt font thickness
    tw, th :: Int32
    V2 tw th = fromSize2i size2i
    thickness = 1

vennCircleA
    :: (PrimMonad m, ToScalar color)
    => MutMat ('S [height, width]) channels depth (PrimState m)
    -> color
    -> Int32
    -> m ()
vennCircleA imgM color thickness =
    circle imgM (V2 100 100 :: V2 Int32) 90 color thickness LineType_AA 0

vennCircleB
    :: (PrimMonad m, ToScalar color)
    => MutMat ('S [height, width]) channels depth (PrimState m)
    -> color
    -> Int32
    -> m ()
vennCircleB imgM color thickness =
    circle imgM (V2 220 100 :: V2 Int32) 90 color thickness LineType_AA 0

--------------------------------------------------------------------------------

extractExampleImages "src"

--------------------------------------------------------------------------------

main :: IO ()
main = do
    renderExampleImages
    render "birds_512x341.png"    birds_512x341
    render "flower_512x341.png"   flower_512x341
    render "sailboat_512x341.png" sailboat_512x341
    forM_ [minBound .. maxBound] $ \lineType ->
      render (show lineType <> ".png") (lineTypeImg lineType)
    forM_ [minBound .. maxBound] $ \fontFace -> do
      render (show fontFace <> ".png")         (fontImg $ Font fontFace NotSlanted 1)
      render (show fontFace <> "_slanted.png") (fontImg $ Font fontFace Slanted    1)

--------------------------------------------------------------------------------
