{-# language FlexibleInstances #-}
{-# language TypeSynonymInstances #-}
{-# options_ghc -fno-warn-orphans #-}

module Main where

import "base" Data.Int
import "base" Data.Monoid
import "base" Data.Proxy
import "base" Data.Word
import "base" Data.Foldable ( forM_ )
import "base" Foreign.C.Types ( CFloat(..), CDouble(..) )
import "base" Foreign.Storable ( Storable )
import qualified "bytestring" Data.ByteString as B
import "linear" Linear.Matrix ( M23, M33 )
import "linear" Linear.Vector ( (^+^), zero )
import "linear" Linear.V2 ( V2(..) )
import "linear" Linear.V3 ( V3(..) )
import "linear" Linear.V4 ( V4(..) )
import "opencv" OpenCV
import "opencv" OpenCV.Unsafe
import "opencv" OpenCV.Internal.Core.Types.Mat.Marshal ( marshalDepth, unmarshalDepth )
import qualified "repa" Data.Array.Repa as Repa
import "repa" Data.Array.Repa.Index ((:.)((:.)))
import "tasty" Test.Tasty
import "tasty-hunit" Test.Tasty.HUnit as HU
import qualified "tasty-quickcheck" Test.Tasty.QuickCheck as QC (testProperty)
import qualified "QuickCheck" Test.QuickCheck as QC
import           "QuickCheck" Test.QuickCheck ( (==>) )
import "transformers" Control.Monad.Trans.Except
import qualified "vector" Data.Vector as V

main :: IO ()
main = defaultMain $ testGroup "opencv"
    [ testGroup "Calib3d"
      [ HU.testCase "findFundamentalMat - no points" testFindFundamentalMat_noPoints
      , HU.testCase "findFundamentalMat" testFindFundamentalMat
      , HU.testCase "computeCorrespondEpilines" testComputeCorrespondEpilines
      ]
    , testGroup "Core"
      [ testGroup "Iso"
        [ testIso "isoPoint2iV2" (toPoint  :: V2 Int32   -> Point2i) fromPoint
        , testIso "isoPoint2fV2" (toPoint  :: V2 CFloat  -> Point2f) fromPoint
        , testIso "isoPoint2dV2" (toPoint  :: V2 CDouble -> Point2d) fromPoint
        , testIso "isoPoint3iV3" (toPoint  :: V3 Int32   -> Point3i) fromPoint
        , testIso "isoPoint3fV3" (toPoint  :: V3 CFloat  -> Point3f) fromPoint
        , testIso "isoPoint3dV3" (toPoint  :: V3 CDouble -> Point3d) fromPoint
        , testIso "isoVec3fV3"   (toVec    :: V3 CFloat  -> Vec3f  ) fromVec
        , testIso "isoVec4iV4"   (toVec    :: V4 Int32   -> Vec4i  ) fromVec
        , testIso "isoSize2iV2"  (toSize   :: V2 Int32   -> Size2i ) fromSize
        , testIso "isoSize2fV2"  (toSize   :: V2 CFloat  -> Size2f ) fromSize
        , testIso "isoScalarV4"  (toScalar :: V4 CDouble -> Scalar ) fromScalar
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
        [ testGroup "Depth"
          [ HU.testCase "marshal unmarshal" depthMarshalUnmarshal
          , QC.testProperty "unmarshal unknown" depthUnmarshalUnknown
          ]
        , testGroup "Mat"
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
      , testGroup "Structural Analysis and Shape Descriptors"
        [ HU.testCase "findContours" testFindContours
        ]
      , testGroup "Feature Detection"
        [ HU.testCase "houghLinesP"   testHoughLinesP
        ]
      , testGroup "Cascade Classifier"
        [ HU.testCase "newCascadeClassifier algorithm" testNewCascadeClassifierAlgorithm
        , HU.testCase "cascadeClassifierDetectMultiScale Arnold" testCascadeClassifierDetectMultiScaleArnold
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
    points :: V.Vector (V2 CDouble)
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
    points1, points2 :: V.Vector (V2 CDouble)
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
    points1, points2 :: V.Vector (V2 CDouble)
    points1 = V.fromList [V2 x y | x <- [0..9], y <- [0..9]]
    points2 = V.map (^+^ (V2 3 2)) points1

testIso :: (QC.Arbitrary a, Eq a, Show a) => String -> (a -> b) -> (b -> a) -> TestTree
testIso name a_to_b b_to_a = QC.testProperty name $ \a -> a QC.=== (b_to_a . a_to_b) a

rectBasicProperties
    :: V2 Int32 -- ^ tl
    -> V2 Int32 -- ^ size
    -> Bool
rectBasicProperties tl size@(V2 w h) = and
      [ fromPoint (rectTopLeft     rect) == tl
      , fromPoint (rectBottomRight rect) == tl ^+^ size
      , fromSize  (rectSize        rect) == size
      ,           rectArea         rect  == (w  *  h)
      ]
    where
      rect :: Rect2i
      rect = toRect $ HRect tl size

rectContainsProperty :: Point2i -> Rect2i -> Bool
rectContainsProperty point rect = rectContains point rect == myRectContains point rect

myRectContains :: Point2i -> Rect2i -> Bool
myRectContains point rect =
    and [ rx <= px
        , ry <= py

        , px < rx + w
        , py < ry + h
        ]
  where
    px, py :: Int32
    V2 px py = fromPoint point

    rx, ry :: Int32
    V2 rx ry = fromPoint $ rectTopLeft rect

    w, h :: Int32
    V2 w h = fromSize $ rectSize rect

-- | Roundtrip every 'Depth' through the `Int32` encoding.
depthMarshalUnmarshal :: HU.Assertion
depthMarshalUnmarshal =
    forM_ [minBound .. maxBound] $ \depth ->
      assertEqual "" depth (unmarshalDepth . marshalDepth $ depth)

depthUnmarshalUnknown :: Int32 -> QC.Property
depthUnmarshalUnknown n =
    n `notElem` knownEncodings ==> QC.expectFailure (unmarshalDepth n `seq` True)
  where
    knownEncodings = map marshalDepth [minBound .. maxBound]

testMatType
    :: ( ToShapeDS    (Proxy shape)
       , ToChannelsDS (Proxy channels)
       , ToDepthDS    (Proxy depth)
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
    rot2D = getRotationMatrix2D (zero :: V2 CFloat) 0 0

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
    let (kpts, descs) = exceptError $ orbDetectAndCompute orb kikker Nothing
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

testFindContours :: HU.Assertion
testFindContours =
  do lambda <- loadLambda
     edges <- unsafeThaw $ exceptError $ canny 30 20 Nothing CannyNormL1 lambda
     contours <- findContours ContourRetrievalExternal
                              ContourApproximationSimple
                              edges
     assertEqual "Unexpected number of contours found"
                 (length contours)
                 1

testHoughLinesP :: HU.Assertion
testHoughLinesP = do
    building <- loadImg ImreadUnchanged "building.jpg"
    let building' :: Mat ('S ['D, 'D]) 'D ('S Word8)
        building' = exceptError $ coerceMat building
    let edgeImg = exceptError $ canny 50 200 Nothing CannyNormL1 building'
    edgeImgM <- thaw edgeImg
    lineSegments <- houghLinesP 1 (pi / 180) 100 Nothing Nothing edgeImgM
    assertBool "no lines found" (V.length lineSegments > 0)

testNewCascadeClassifierAlgorithm :: HU.Assertion
testNewCascadeClassifierAlgorithm = do
  mbCC <- newCascadeClassifier "/this/is/bogus.xml"
  case mbCC of
    Nothing -> return ()
    Just _cc -> fail "expected Nothing from newCascadeClassifier"

testCascadeClassifierDetectMultiScaleArnold :: HU.Assertion
testCascadeClassifierDetectMultiScaleArnold = do
    Just ccFrontal <- newCascadeClassifier "data/haarcascade_frontalface_default.xml"
    Just ccEyes <- newCascadeClassifier "data/haarcascade_eye.xml"
    arnold :: Mat ('S ['D, 'D]) ('S 3) ('S Word8) <-
      exceptError . coerceMat <$> loadImg ImreadUnchanged "arnold-schwarzenegger.jpg"
    let arnoldGray :: Mat ('S ['D, 'D]) ('S 1) ('S Word8) = exceptError (cvtColor bgr gray arnold)
    -- OpenCV detects the left eye twice for this pic.
    let arnoldEyes =
          cascadeClassifierDetectMultiScale ccEyes Nothing Nothing (Nothing :: Maybe (V2 Int32)) (Nothing :: Maybe (V2 Int32)) arnoldGray
    assertBool "unexpected number of eyes detected" (V.length arnoldEyes == 3)
    let arnoldFront =
          cascadeClassifierDetectMultiScale ccFrontal Nothing Nothing (Nothing :: Maybe (V2 Int32)) (Nothing :: Maybe (V2 Int32)) arnoldGray
    assertBool "unexpected number of faces detected" (V.length arnoldFront == 1)

type Lambda = Mat (ShapeT [256, 256]) ('S 1) ('S Word8)

loadLambda :: IO Lambda
loadLambda =
  fmap (exceptError . coerceMat . imdecode ImreadGrayscale)
       (B.readFile "data/lambda.png")

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
testMatToM23 m v = assertEqual "" v $ fromMat m

testMatToM33
    :: (Eq e, Show e, Storable e)
    => Mat (ShapeT [3, 3]) ('S 1) ('S e)
    -> V3 (V3 e)
    -> HU.Assertion
testMatToM33 m v = assertEqual "" v $ fromMat m

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

instance QC.Arbitrary CFloat where
    arbitrary = CFloat <$> QC.arbitrary

instance QC.Arbitrary CDouble where
    arbitrary = CDouble <$> QC.arbitrary

instance (QC.Arbitrary a) => QC.Arbitrary (V2 a) where
    arbitrary = V2 <$> QC.arbitrary <*> QC.arbitrary

instance (QC.Arbitrary a) => QC.Arbitrary (V3 a) where
    arbitrary = V3 <$> QC.arbitrary <*> QC.arbitrary <*> QC.arbitrary

instance (QC.Arbitrary a) => QC.Arbitrary (V4 a) where
    arbitrary = V4 <$> QC.arbitrary <*> QC.arbitrary <*> QC.arbitrary <*> QC.arbitrary

instance (QC.Arbitrary a) => QC.Arbitrary (HRect a) where
    arbitrary = HRect <$> QC.arbitrary <*> QC.arbitrary

instance QC.Arbitrary Rect2i where
    arbitrary = toRect <$> (QC.arbitrary :: QC.Gen (HRect Int32))

instance QC.Arbitrary Point2i where
    arbitrary = toPoint <$> (QC.arbitrary :: QC.Gen (V2 Int32))

--------------------------------------------------------------------------------

assertNull
    :: [a] -- ^ List that should be empty.
    -> ([a] -> String) -- ^ Error when the list is not empty.
    -> IO ()
assertNull [] _showError = pure ()
assertNull xs  showError = assertFailure $ showError xs
