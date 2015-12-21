{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import           "lumi-hackage-extended" Lumi.Prelude
import qualified "bytestring"            Data.ByteString as B
import           "lens"                  Control.Lens
import           "linear"                Linear.V2 ( V2(..) )
import           "linear"                Linear.V3 ( V3(..) )
import           "linear"                Linear.V4 ( V4(..) )
import qualified "repa"                  Data.Array.Repa as Repa
import           "tasty"                 Test.Tasty
import           "tasty-hunit"           Test.Tasty.HUnit      as HU
import qualified "tasty-quickcheck"      Test.Tasty.QuickCheck as QC (testProperty)
import qualified "QuickCheck"            Test.QuickCheck       as QC
import           "thea"                  OpenCV
import           "this"                  Paths_thea ( getDataFileName )

-- import            "base"            Debug.Trace

main :: IO ()
main = defaultMain $ testGroup "thea"
    [ testGroup "Core"
      [ testGroup "Iso"
        [ testIso "isoPoint2iV2" (isoPoint2iV2 :: Iso' Point2i (V2 Int))
        , testIso "isoPoint2fV2" (isoPoint2fV2 :: Iso' Point2f (V2 Float))
        , testIso "isoPoint2dV2" (isoPoint2dV2 :: Iso' Point2d (V2 Double))
        , testIso "isoPoint3iV3" (isoPoint3iV3 :: Iso' Point3i (V3 Int))
        , testIso "isoPoint3fV3" (isoPoint3fV3 :: Iso' Point3f (V3 Float))
        , testIso "isoPoint3dV3" (isoPoint3dV3 :: Iso' Point3d (V3 Double))

        , testIso "isoSize2iV2"  (isoSize2iV2  :: Iso' Size2i (V2 Int))
        , testIso "isoSize2fV2"  (isoSize2fV2  :: Iso' Size2f (V2 Float))
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
        [
          testGroup "Repa"
          [
            -- FIXME (BvD): Finish this test case!
            HU.testCase "toRepa" $ do
              emptyMat <- newEmptyMat
              case emptyMat ^? repa :: Maybe (Repa.Array M Repa.DIM0 Int) of
                Nothing -> assertFailure "failure"
                Just _repaArray -> pure ()
          ]
        ]
      ]
    , testGroup "ImgProc"
      [
      ]
    , testGroup "ImgCodecs"
      [ testGroup "imencode . imdecode"
        [ HU.testCase "OutputBmp"      $ encodeDecode OutputBmp
        , HU.testCase "OutputExr"      $ encodeDecode OutputExr
        , HU.testCase "OutputHdr"      $ encodeDecode (OutputHdr True)
        , HU.testCase "OutputJpeg"     $ encodeDecode (OutputJpeg defaultJpegParams)
        , HU.testCase "OutputJpeg2000" $ encodeDecode OutputJpeg2000
        , HU.testCase "OutputPng"      $ encodeDecode (OutputPng defaultPngParams)
        , HU.testCase "OutputPxm"      $ encodeDecode (OutputPxm True)
        , HU.testCase "OutputSunras"   $ encodeDecode OutputSunras
        , HU.testCase "OutputTiff"     $ encodeDecode OutputTiff
        , HU.testCase "OutputWebP"     $ encodeDecode (OutputWebP 100)
        ]
      ]
    , testGroup "HighGui"
      [
      ]
    , testGroup "Video"
      [
      ]
    ]


testIso :: (QC.Arbitrary b, Eq b, Show b) => String -> Iso' a b -> TestTree
testIso name myIso = QC.testProperty name $ \b -> b QC.=== (b ^. from myIso . myIso)

rectBasicProperties
    :: Int -- ^ x
    -> Int -- ^ y
    -> Int -- ^ width
    -> Int -- ^ height
    -> Bool
rectBasicProperties x y w h = and
      [ (rectTopLeft     rect ^. isoPoint2iV2) == V2  x     y
      , (rectBottomRight rect ^. isoPoint2iV2) == V2 (x+w) (y+h)
      , (rectSize        rect ^. isoSize2iV2)  == V2    w     h
      ,  rectArea        rect                  ==      (w  *  h)
      ]
    where
      rect = mkRect x y w h

rectContainsProperty :: Point2i -> Rect -> Bool
rectContainsProperty point rect = rectContains point rect == myRectContains point rect

myRectContains :: Point2i -> Rect -> Bool
myRectContains point rect =
    and [ px >= rx
        , py >= ry

        , px < rx + w
        , py < ry + h

        , w > 0
        , h > 0
        ]
  where
    px, py :: Int
    V2 px py = point ^. isoPoint2iV2

    rx, ry :: Int
    V2 rx ry = rectTopLeft rect ^. isoPoint2iV2

    w, h :: Int
    V2 w h = rectSize rect ^. isoSize2iV2

encodeDecode :: OutputFormat -> HU.Assertion
encodeDecode outputFormat = do
    lennaFp <- getDataFileName "Lenna.png"
    bs1 <- B.readFile lennaFp

    let mat1 = imdecode ImreadUnchanged bs1

        bs2  = either throw id $ imencode outputFormat mat1
        mat2 = imdecode ImreadUnchanged bs2

        bs3  = either throw id $ imencode outputFormat mat2

    assertBool "imencode . imdecode failure"
               (bs2 == bs3)


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
    arbitrary = mkRect <$> QC.arbitrary <*> QC.arbitrary <*> QC.arbitrary <*> QC.arbitrary

instance QC.Arbitrary Point2i where
    arbitrary = view (from isoPoint2iV2) <$> (QC.arbitrary :: QC.Gen (V2 Int))
