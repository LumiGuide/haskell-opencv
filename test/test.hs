{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import "base" Data.Int
import "base" Data.Monoid
import "base" Data.Proxy
import "base" Data.Word
import "base" Foreign.Storable ( Storable )
import qualified "bytestring" Data.ByteString as B
import "linear" Linear.Matrix ( M23, M33 )
import "linear" Linear.Vector ( (^+^), zero )
import "linear" Linear.V2 ( V2(..) )
import "linear" Linear.V3 ( V3(..) )
import "linear" Linear.V4 ( V4(..) )
import qualified "repa" Data.Array.Repa as Repa
import "repa" Data.Array.Repa.Index ((:.)((:.)))
import "tasty" Test.Tasty
import "tasty-hunit" Test.Tasty.HUnit as HU
import qualified "tasty-quickcheck" Test.Tasty.QuickCheck as QC (testProperty)
import "thea" OpenCV
import "thea" OpenCV.Unsafe
import qualified "QuickCheck" Test.QuickCheck as QC
import "transformers" Control.Monad.Trans.Except
import qualified "vector" Data.Vector as V

main :: IO ()
main = defaultMain $ testGroup "thea"
    [ testGroup "Calib3d"
      [ HU.testCase "findFundamentalMat - no points" testFindFundamentalMat_noPoints
      , HU.testCase "findFundamentalMat" testFindFundamentalMat
      , HU.testCase "computeCorrespondEpilines" testComputeCorrespondEpilines
      ]
    , testGroup "Core"
      [ testGroup "Iso"
        [ testIso "isoPoint2iV2" (convert :: V2 Int32  -> Point2i) convert
        , testIso "isoPoint2fV2" (convert :: V2 Float  -> Point2f) convert
        , testIso "isoPoint2dV2" (convert :: V2 Double -> Point2d) convert
        , testIso "isoPoint3iV3" (convert :: V3 Int32  -> Point3i) convert
        , testIso "isoPoint3fV3" (convert :: V3 Float  -> Point3f) convert
        , testIso "isoPoint3dV3" (convert :: V3 Double -> Point3d) convert
        , testIso "isoSize2iV2"  (convert :: V2 Int32  -> Size2i ) convert
        , testIso "isoSize2fV2"  (convert :: V2 Float  -> Size2f ) convert
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
      , testGroup "Types"
        [ testGroup "Mat"
          [ HU.testCase "emptyMat" $ testMatType emptyMat
          , testGroup "matInfo"
            [ matHasInfoFP "Lenna.png"  $ MatInfo [512, 512] Depth_8U 3
            , matHasInfoFP "kikker.jpg" $ MatInfo [390, 500] Depth_8U 3
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
    , testGroup "Features2d"
      [ HU.testCase "orbDetectAndCompute" testOrbDetectAndCompute
      ]
    , testGroup "HighGui"
      [
      ]
    , testGroup "Video"
      [
      ]
    ]

testFindFundamentalMat_noPoints :: HU.Assertion
testFindFundamentalMat_noPoints =
    case runExcept $ findFundamentalMat points points (FM_Ransac Nothing Nothing) of
      Left err -> assertFailure (show err)
      Right Nothing -> pure ()
      Right (Just _) -> assertFailure "result despite lack of data"
  where
    points :: V.Vector (V2 Double)
    points = V.empty

testFindFundamentalMat :: HU.Assertion
testFindFundamentalMat =
    case runExcept $ findFundamentalMat points1 points2 (FM_Ransac Nothing Nothing) of
      Left err -> assertFailure (show err)
      Right Nothing -> assertFailure "couldn't find fundamental matrix"
      Right (Just (fm, pointsMask)) -> do
          assertNull (typeCheckMat fm        ) (("fm: "         <>) . show)
          assertNull (typeCheckMat pointsMask) (("pointsMask: " <>) . show)
  where
    points1, points2 :: V.Vector (V2 Double)
    points1 = V.fromList [V2 x y | x <- [0..9], y <- [0..9]]
    points2 = V.map (^+^ (V2 3 2)) points1

testComputeCorrespondEpilines :: HU.Assertion
testComputeCorrespondEpilines =
    case runExcept $ findFundamentalMat points1 points2 (FM_Ransac Nothing Nothing) of
      Right (Just (fm, _pointsMask)) ->
          let fm' = unsafeCoerceMat fm
          in case runExcept $ computeCorrespondEpilines points1 Image1 fm' of
               Left err1 -> assertFailure (show err1)
               Right epilines1 -> do
                 assertNull (typeCheckMat epilines1) (("epilines1: " <>) . show)
                 case runExcept $ computeCorrespondEpilines points2 Image2 fm' of
                   Left err2 -> assertFailure (show err2)
                   Right epilines2 ->
                       assertNull (typeCheckMat epilines2) (("epilines2: " <>) . show)
      _ -> assertFailure "couldn't find fundamental matrix"
  where
    points1, points2 :: V.Vector (V2 Double)
    points1 = V.fromList [V2 x y | x <- [0..9], y <- [0..9]]
    points2 = V.map (^+^ (V2 3 2)) points1

testIso :: (QC.Arbitrary a, Eq a, Show a) => String -> (a -> b) -> (b -> a) -> TestTree
testIso name a_to_b b_to_a = QC.testProperty name $ \a -> a QC.=== (b_to_a . a_to_b) a

rectBasicProperties
    :: V2 Int32 -- ^ tl
    -> V2 Int32 -- ^ size
    -> Bool
rectBasicProperties tl size@(V2 w h) = and
      [ convert (rectTopLeft     rect) == tl
      , convert (rectBottomRight rect) == tl ^+^ size
      , convert (rectSize        rect) == size
      ,          rectArea        rect  == (w  *  h)
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
    V2 px py = convert point

    rx, ry :: Int32
    V2 rx ry = convert $ rectTopLeft rect

    w, h :: Int32
    V2 w h = convert $ rectSize rect

testMatType
    :: ( Convert (Proxy shape)    (DS [DS Int32])
       , Convert (Proxy channels) (DS Int32)
       , Convert (Proxy depth)    (DS Depth)
       )
    => Mat shape channels depth
    -> HU.Assertion
testMatType mat =
    case typeCheckMat mat of
      [] -> pure ()
      errors -> assertFailure $ show errors

matHasInfoFP :: FilePath -> MatInfo -> TestTree
matHasInfoFP fp expectedInfo = HU.testCase fp $ do
    mat <- loadImg ImreadUnchanged fp
    assertEqual "" expectedInfo (matInfo mat)

testGetRotationMatrix2D :: HU.Assertion
testGetRotationMatrix2D = testMatType rot2D
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

    let bs2  = exceptError $ imencode outputFormat mat1
        mat2 = imdecode ImreadUnchanged bs2

        bs3  = exceptError $ imencode outputFormat mat2

    assertBool "imencode . imdecode failure"
               (bs2 == bs3)

testOrbDetectAndCompute :: HU.Assertion
testOrbDetectAndCompute = do
    kikker <- loadImg ImreadUnchanged "kikker.jpg"
    let (kpts, descs) = orbDetectAndCompute orb kikker Nothing
        kptsRec  = V.map keyPointAsRec kpts
        kpts2    = V.map mkKeyPoint    kptsRec
        kptsRec2 = V.map keyPointAsRec kpts2
        numDescs = head $ miShape $ matInfo descs
    assertEqual "kpt conversion failure"
                kptsRec
                kptsRec2
    assertEqual "number of kpts /= number of descriptors"
                (fromIntegral $ V.length kpts)
                numDescs
  where
    orb = mkOrb defaultOrbParams {orb_nfeatures = 10}

loadImg :: ImreadMode -> FilePath -> IO (Mat ('S ['D, 'D]) 'D 'D)
loadImg readMode fp = imdecode readMode <$> B.readFile ("data/" <> fp)

imgToRepa :: HU.Assertion
imgToRepa = do
    mat <- loadImg ImreadUnchanged "kikker.jpg"
    case runExcept $ coerceMat mat of
      Left err -> assertFailure $ show err
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
    arbitrary = convert <$> (QC.arbitrary :: QC.Gen (V2 Int32))

--------------------------------------------------------------------------------

assertNull
    :: [a] -- ^ List that should be empty.
    -> ([a] -> String) -- ^ Error when the list is not empty.
    -> IO ()
assertNull [] _showError = pure ()
assertNull xs  showError = assertFailure $ showError xs
