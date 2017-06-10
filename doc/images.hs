{-# language TemplateHaskell #-}
{-# language OverloadedStrings #-}
{-# language FlexibleContexts #-}
{-# language CPP #-}

module Main where

import "base" Data.Functor ( void )
import "base" Data.Foldable ( forM_ )
import "base" Data.Int
import "base" Data.Monoid ( (<>) )
import "base" Data.Proxy
import "base" Data.Traversable
import "base" Data.Word
import "base" Foreign.C.Types ( CFloat )
import "base" GHC.TypeLits
import "base" System.IO.Unsafe ( unsafePerformIO )
import "data-default" Data.Default
import qualified "bytestring" Data.ByteString as B
import qualified "JuicyPixels" Codec.Picture
import qualified "linear" Linear.Metric as Linear ( norm )
import "linear" Linear ( M33 )
import "linear" Linear.Vector ( (^+^), (^-^) )
import "linear" Linear.V2 ( V2(..) )
import "linear" Linear.V3 ( V3(..) )
import "linear" Linear.V4 ( V4(..) )
import "opencv" OpenCV
import qualified "opencv" OpenCV.Juicy
import "opencv" OpenCV.Unsafe
#ifdef HAVE_OPENCV_EXTRA
import "opencv-extra" OpenCV.Extra
#endif
import "primitive" Control.Monad.Primitive ( PrimMonad, PrimState )
import qualified "text" Data.Text as T
import qualified "vector" Data.Vector as V
import "transformers" Control.Monad.Trans.Class ( lift )

import "this" ExampleExtractor

--------------------------------------------------------------------------------

transparent, white, black, blue, green, red :: Scalar
transparent = toScalar (V4 255 255 255   0 :: V4 Double)
white       = toScalar (V4 255 255 255 255 :: V4 Double)
black       = toScalar (V4   0   0   0 255 :: V4 Double)
blue        = toScalar (V4 255   0   0 255 :: V4 Double)
green       = toScalar (V4   0 255   0 255 :: V4 Double)
red         = toScalar (V4   0   0 255 255 :: V4 Double)

type Birds_768x512    = Mat (ShapeT [512, 768]) ('S 3) ('S Word8)
type Flower_768x512   = Mat (ShapeT [512, 768]) ('S 3) ('S Word8)
type Sailboat_768x512 = Mat (ShapeT [512, 768]) ('S 3) ('S Word8)
type Bikes_768x512    = Mat (ShapeT [512, 768]) ('S 3) ('S Word8)
type Birds_512x341    = Mat (ShapeT [341, 512]) ('S 3) ('S Word8)
type Flower_512x341   = Mat (ShapeT [341, 512]) ('S 3) ('S Word8)
type Sailboat_512x341 = Mat (ShapeT [341, 512]) ('S 3) ('S Word8)
type Bikes_512x341    = Mat (ShapeT [341, 512]) ('S 3) ('S Word8)
type Frog             = Mat (ShapeT [390, 500]) ('S 3) ('S Word8)
type Lambda           = Mat (ShapeT [256, 256]) ('S 1) ('S Word8)
type Circles_1000x625 = Mat (ShapeT [625, 1000]) ('S 3) ('S Word8)
type Building_868x600 = Mat (ShapeT [600, 868]) ('S 3) ('S Word8)
type DamageMask       = Mat (ShapeT [341, 512]) ('S 1) ('S Word8)
type Lenna_512x512    = Mat (ShapeT [512, 512]) ('S 3) ('S Word8)
type Arnold           = Mat (ShapeT [3504, 2336]) ('S 3) ('S Word8)
type Arnold_small     = Mat (ShapeT [ 900,  600]) ('S 3) ('S Word8)

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

bikes_768x512 :: Sailboat_768x512
bikes_768x512 =
    exceptError $ coerceMat $ unsafePerformIO $
      imdecode ImreadUnchanged <$> B.readFile "data/kodim05.png"

damageMask :: DamageMask
damageMask =
    exceptError $ coerceMat $ unsafePerformIO $
      imdecode ImreadUnchanged <$> B.readFile "data/damage_mask.png"

smallerKodakImg
    :: Mat (ShapeT [512, 768]) ('S 3) ('S Word8)
    -> Mat (ShapeT [341, 512]) ('S 3) ('S Word8)
smallerKodakImg img =
    exceptError $ coerceMat =<<
      resize (ResizeAbs $ toSize (V2 512 341 :: V2 Int32))
             InterArea
             img

birds_512x341 :: Birds_512x341
birds_512x341 = smallerKodakImg birds_768x512

flower_512x341 :: Flower_512x341
flower_512x341 = smallerKodakImg flower_768x512

sailboat_512x341 :: Flower_512x341
sailboat_512x341 = smallerKodakImg sailboat_768x512

bikes_512x341 :: Flower_512x341
bikes_512x341 = smallerKodakImg bikes_768x512

frog :: Frog
frog =
    exceptError $ coerceMat $ unsafePerformIO $
      imdecode ImreadUnchanged <$> B.readFile "data/kikker.jpg"

lambda :: Lambda
lambda =
    exceptError $ coerceMat $ unsafePerformIO $
      imdecode ImreadGrayscale <$> B.readFile "data/lambda.png"

circles_1000x625 :: Circles_1000x625
circles_1000x625 =
    exceptError $ coerceMat $ unsafePerformIO $
      imdecode ImreadUnchanged <$> B.readFile "data/circles.png"

building_868x600 :: Building_868x600
building_868x600 =
    exceptError $ coerceMat $ unsafePerformIO $
      imdecode ImreadUnchanged <$> B.readFile "data/building.jpg"

lenna_512x512 :: Lenna_512x512
lenna_512x512 =
    exceptError $ coerceMat $ unsafePerformIO $
      imdecode ImreadUnchanged <$> B.readFile "data/Lenna.png"

arnold :: Arnold
arnold =
    exceptError $ coerceMat $ unsafePerformIO $
      imdecode ImreadUnchanged <$> B.readFile "data/arnold-schwarzenegger.jpg"

arnold_small :: Arnold_small
arnold_small =
    exceptError $ coerceMat =<<
      resize (ResizeAbs $ toSize (V2 600 900 :: V2 Int32))
             InterArea
             arnold

--------------------------------------------------------------------------------

type CarOverhead = Animation (ShapeT [240, 320]) ('S 3) ('S Word8)

carOverhead :: CarOverhead
carOverhead = unsafePerformIO $ loadAnimation 4 "data/car-overhead-3.mp4"

--------------------------------------------------------------------------------

loadAnimation
    :: (ToNatDS (Proxy h), ToNatDS (Proxy w))
    => Int
    -> FilePath
    -> IO (Animation ('S [h, w]) ('S 3) ('S Word8))
loadAnimation delay fp = do
    cap <- newVideoCapture
    exceptErrorIO $ videoCaptureOpen cap (VideoFileSource fp Nothing)
    frames <- grabFrames cap []
    exceptErrorIO $ videoCaptureRelease cap
    pure $ map (delay, ) frames
  where
    grabFrames cap frames = do
        ok <- videoCaptureGrab cap
        if ok
        then do
          mbFrameRaw <- videoCaptureRetrieve cap
          case mbFrameRaw of
            Nothing -> pure $ reverse frames
            Just frameRaw -> do
              frame <- exceptErrorIO $ pureExcept $ coerceMat frameRaw
              grabFrames cap (frame : frames)
        else pure $ reverse frames

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
           =<< matSubRect img (toRect $ HRect (pure p) (V2 w h))
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
    V2 tw th = fromSize size2i
    thickness = 1

vennCircleA
    :: (PrimMonad m, ToScalar color)
    => Mut (Mat ('S [height, width]) channels depth) (PrimState m)
    -> color
    -> Int32
    -> m ()
vennCircleA imgM color thickness =
    circle imgM (V2 100 100 :: V2 Int32) 90 color thickness LineType_AA 0

vennCircleB
    :: (PrimMonad m, ToScalar color)
    => Mut (Mat ('S [height, width]) channels depth) (PrimState m)
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
    renderImage "birds_512x341.png"    birds_512x341
    renderImage "flower_512x341.png"   flower_512x341
    renderImage "sailboat_512x341.png" sailboat_512x341
    renderImage "bikes_512x341.png"    bikes_512x341
    forM_ [minBound .. maxBound] $ \lineType ->
      renderImage (show lineType <> ".png") (lineTypeImg lineType)
    forM_ [minBound .. maxBound] $ \fontFace -> do
      renderImage (show fontFace <> ".png")         (fontImg $ Font fontFace NotSlanted 1)
      renderImage (show fontFace <> "_slanted.png") (fontImg $ Font fontFace Slanted    1)

--------------------------------------------------------------------------------
