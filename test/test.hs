{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import qualified "bytestring" Data.ByteString as B
import "linear" Linear.Matrix ( M23, M33 )
import "linear" Linear.Vector ( (^+^), zero )
import "linear" Linear.V2 ( V2(..) )
import "linear" Linear.V3 ( V3(..) )
import "linear" Linear.V4 ( V4(..) )
import "lumi-hackage-extended" Lumi.Prelude
import qualified "repa" Data.Array.Repa as Repa
import "repa" Data.Array.Repa.Index ((:.)((:.)))
import "tasty" Test.Tasty
import "tasty-hunit" Test.Tasty.HUnit as HU
import qualified "tasty-quickcheck" Test.Tasty.QuickCheck as QC (testProperty)
import "thea" OpenCV
import qualified "QuickCheck" Test.QuickCheck as QC

main :: IO ()
main = defaultMain $ testGroup "thea"
    [ testGroup "Core"
      [ testGroup "Iso"
        [ testIso "isoPoint2iV2" (toPoint2i :: V2 Int32  -> Point2i) fromPoint2i
        , testIso "isoPoint2fV2" (toPoint2f :: V2 Float  -> Point2f) fromPoint2f
        , testIso "isoPoint2dV2" (toPoint2d :: V2 Double -> Point2d) fromPoint2d
        , testIso "isoPoint3iV3" (toPoint3i :: V3 Int32  -> Point3i) fromPoint3i
        , testIso "isoPoint3fV3" (toPoint3f :: V3 Float  -> Point3f) fromPoint3f
        , testIso "isoPoint3dV3" (toPoint3d :: V3 Double -> Point3d) fromPoint3d
        , testIso "isoSize2iV2"  (toSize2i  :: V2 Int32  -> Size2i ) fromSize2i
        , testIso "isoSize2fV2"  (toSize2f  :: V2 Float  -> Size2f ) fromSize2f
        ]
      , testGroup "Rect"
        [ QC.testProperty "basic-properties" rectBasicProperties
        , QC.testProperty "rectContains" rectContainsProperty
        ]
      , testGroup "RotatedRect"
        [
        ]
      , testGroup "Scalar"
        [
        ]
      , testGroup "Mat"
        [ testGroup "matInfo"
          [ matHasInfoFP "Lenna.png"  $ MatInfo [512, 512] MatDepth_8U 3
          , matHasInfoFP "kikker.jpg" $ MatInfo [390, 500] MatDepth_8U 3
          ]
        , testGroup "HMat"
          [ HU.testCase "hElemsSize" $ hmatElemSize "Lenna.png" (512 * 512 * 3)
          -- , HU.testCase "eye 33" $ assertEqual "" (HMat [3,3] 1 $ HElems_8U $ VU.fromList [1,0,0, 0,1,0, 0,0,1]) $ eye33_c1 ^. hmat
          , testGroup "mat -> hmat -> mat -> hmat"
            [ HU.testCase "eye 33 - 1 channel"  $ hMatEncodeDecode eye33_8u_1c
            , HU.testCase "eye 22 - 3 channels" $ hMatEncodeDecode eye22_8u_3c
            , hMatEncodeDecodeFP "Lenna.png"
            , hMatEncodeDecodeFP "kikker.jpg"
            ]
          ]
        , testGroup "Repa"
          [ HU.testCase "imgToRepa" imgToRepa ]
        , testGroup "fixed size matrices"
          [ HU.testCase "M23 eye" $ testMatToM23 eye23_8u_1c (eye_m23 :: M23 Word8)
          , HU.testCase "M33 eye" $ testMatToM33 eye33_8u_1c (eye_m33 :: M33 Word8)
          ]
        ]
      ]
    , testGroup "ImgProc"
      [ testGroup "GeometricImgTransform"
        [ HU.testCase "getRotationMatrix2D" testGetRotationMatrix2D
        ]
      ]
    , testGroup "ImgCodecs"
      [ testGroup "imencode . imdecode"
        [ HU.testCase "OutputBmp"      $ encodeDecode OutputBmp
-- !?!?!, HU.testCase "OutputExr"      $ encodeDecode OutputExr
        , HU.testCase "OutputHdr"      $ encodeDecode (OutputHdr True)
-- !?!?!, HU.testCase "OutputJpeg"     $ encodeDecode (OutputJpeg defaultJpegParams)
        , HU.testCase "OutputJpeg2000" $ encodeDecode OutputJpeg2000
        , HU.testCase "OutputPng"      $ encodeDecode (OutputPng defaultPngParams)
        , HU.testCase "OutputPxm"      $ encodeDecode (OutputPxm True)
        , HU.testCase "OutputSunras"   $ encodeDecode OutputSunras
        , HU.testCase "OutputTiff"     $ encodeDecode OutputTiff
-- !?!?!, HU.testCase "OutputWebP"     $ encodeDecode (OutputWebP 100)
        ]
      ]
    , testGroup "HighGui"
      [
      ]
    , testGroup "Video"
      [
      ]
    ]


testIso :: (QC.Arbitrary a, Eq a, Show a) => String -> (a -> b) -> (b -> a) -> TestTree
testIso name a_to_b b_to_a = QC.testProperty name $ \a -> a QC.=== (b_to_a . a_to_b) a

rectBasicProperties
    :: V2 Int32 -- ^ tl
    -> V2 Int32 -- ^ size
    -> Bool
rectBasicProperties tl size@(V2 w h) = and
      [ fromPoint2i (rectTopLeft     rect) == tl
      , fromPoint2i (rectBottomRight rect) == tl ^+^ size
      , fromSize2i  (rectSize        rect) == size
      ,              rectArea        rect  == (w  *  h)
      ]
    where
      rect = mkRect tl size

rectContainsProperty :: Point2i -> Rect -> Bool
rectContainsProperty point rect = rectContains point rect == myRectContains point rect

myRectContains :: Point2i -> Rect -> Bool
myRectContains point rect =
    and [ rx <= px
        , ry <= py

        , px < rx + w
        , py < ry + h
        ]
  where
    px, py :: Int32
    V2 px py = fromPoint2i point

    rx, ry :: Int32
    V2 rx ry = fromPoint2i $ rectTopLeft rect

    w, h :: Int32
    V2 w h = fromSize2i $ rectSize rect

matHasInfoFP :: FilePath -> MatInfo -> TestTree
matHasInfoFP fp expectedInfo = HU.testCase fp $ do
    mat <- loadImg ImreadUnchanged fp
    assertEqual "" expectedInfo (matInfo mat)

testGetRotationMatrix2D :: HU.Assertion
testGetRotationMatrix2D =
    case coerceMat rot2D `asTypeOf` Right rot2D of
      Left errors -> assertFailure (intercalate ", " errors)
      Right _ -> return ()
  where
    rot2D = getRotationMatrix2D (zero :: V2 Float) 0 0

hMatEncodeDecodeFP :: FilePath -> TestTree
hMatEncodeDecodeFP fp = HU.testCase fp $ do
    mat <- loadImg ImreadUnchanged fp
    hMatEncodeDecode mat

hMatEncodeDecode :: Mat shape channels depth -> HU.Assertion
hMatEncodeDecode m1 =
    assertEqual "" h1 h2
    -- assertBool "mat hmat conversion failure" (h1 == h2)
  where
    h1 = matToHMat m1
    m2 = hMatToMat h1
    h2 = matToHMat m2

hmatElemSize :: FilePath -> Int -> HU.Assertion
hmatElemSize fp expectedElemSize = do
  mat <- loadImg ImreadUnchanged fp
  assertEqual "" expectedElemSize $ hElemsLength $ hmElems $ matToHMat mat

encodeDecode :: OutputFormat -> HU.Assertion
encodeDecode outputFormat = do
    mat1 <- loadImg ImreadUnchanged "Lenna.png"

    let bs2  = either throw id $ imencode outputFormat mat1
        mat2 = imdecode ImreadUnchanged bs2

        bs3  = either throw id $ imencode outputFormat mat2

    assertBool "imencode . imdecode failure"
               (bs2 == bs3)

loadImg :: ImreadMode -> FilePath -> IO (Mat ('S ['D, 'D]) 'D 'D)
loadImg readMode fp = imdecode readMode <$> B.readFile ("data/" <> fp)

imgToRepa :: HU.Assertion
imgToRepa = do
    mat <- loadImg ImreadUnchanged "kikker.jpg"
    case coerceMat mat of
      Left errors -> assertFailure (intercalate ", " errors)
      Right (mat' :: Mat ('S '[ 'D, 'D ]) ('S 3) ('S Word8)) -> do
        let repaArray :: Repa.Array (M '[ 'D, 'D ] 3) Repa.DIM3 Word8
            repaArray = toRepa mat'
        assertEqual "extent" (Repa.Z :. 3 :. 500 :. 390 ) (Repa.extent repaArray)

testMatToM23
    :: (Eq e, Show e, Storable e)
    => Mat (ShapeT [2, 3]) ('S 1) ('S e)
    -> V2 (V3 e)
    -> HU.Assertion
testMatToM23 m v = assertEqual "" v $ matToM23 m

testMatToM33
    :: (Eq e, Show e, Storable e)
    => Mat (ShapeT [3, 3]) ('S 1) ('S e)
    -> V3 (V3 e)
    -> HU.Assertion
testMatToM33 m v = assertEqual "" v $ matToM33 m

--------------------------------------------------------------------------------

eye23_8u_1c :: Mat (ShapeT [2, 3]) ('S 1) ('S Word8)
eye33_8u_1c :: Mat (ShapeT [3, 3]) ('S 1) ('S Word8)
eye22_8u_3c :: Mat (ShapeT [2, 2]) ('S 3) ('S Word8)

eye23_8u_1c = eyeMat (Proxy :: Proxy 2) (Proxy :: Proxy 3) (Proxy :: Proxy 1) (Proxy :: Proxy Word8)
eye33_8u_1c = eyeMat (Proxy :: Proxy 3) (Proxy :: Proxy 3) (Proxy :: Proxy 1) (Proxy :: Proxy Word8)
eye22_8u_3c = eyeMat (Proxy :: Proxy 2) (Proxy :: Proxy 2) (Proxy :: Proxy 3) (Proxy :: Proxy Word8)

eye_m23 :: (Num e) => M23 e
eye_m33 :: (Num e) => M33 e

eye_m23 = V2 (V3 1 0 0) (V3 0 1 0)
eye_m33 = V3 (V3 1 0 0) (V3 0 1 0) (V3 0 0 1)

--------------------------------------------------------------------------------
-- QuikcCheck Arbitrary Instances
--------------------------------------------------------------------------------

instance (QC.Arbitrary a) => QC.Arbitrary (V2 a) where
    arbitrary = V2 <$> QC.arbitrary <*> QC.arbitrary

instance (QC.Arbitrary a) => QC.Arbitrary (V3 a) where
    arbitrary = V3 <$> QC.arbitrary <*> QC.arbitrary <*> QC.arbitrary

instance (QC.Arbitrary a) => QC.Arbitrary (V4 a) where
    arbitrary = V4 <$> QC.arbitrary <*> QC.arbitrary <*> QC.arbitrary <*> QC.arbitrary

instance QC.Arbitrary Rect where
    arbitrary = mkRect <$> QC.arbitrary <*> QC.arbitrary

instance QC.Arbitrary Point2i where
    arbitrary = toPoint2i <$> (QC.arbitrary :: QC.Gen (V2 Int32))
