{-# language FlexibleInstances #-}
{-# language LambdaCase #-}
{-# language TypeApplications #-}
{-# language TypeSynonymInstances #-}
{-# language TypeFamilies #-}
{-# language CPP #-}
{-# options_ghc -fno-warn-orphans #-}

module Main where

import "base" Control.Exception ( Exception, try, displayException, evaluate )
import "base" Control.Monad.IO.Class ( liftIO )
import "base" Data.Functor ( ($>) )
import "base" Data.Int
import "base" Data.Proxy
import "base" Data.Word
import "base" Data.List.NonEmpty ( nonEmpty )
import "base" Data.Monoid ( (<>) )
import "base" Data.Foldable ( for_, toList )
import "base" Foreign.C.Types ( CFloat(..), CDouble(..) )
import "base" Foreign.Storable ( Storable )
import "base" System.Environment ( setEnv )
import qualified "bytestring" Data.ByteString as B
import "deepseq" Control.DeepSeq ( NFData, force )
import "lens" Control.Lens.Combinators ( view )
import "linear" Linear.Matrix ( M23, M33, (!*) )
import qualified "linear" Linear as L ( Metric(..) )
import "linear" Linear.Vector ( (^+^), zero )
import "linear" Linear.V2 ( V2(..) )
import "linear" Linear.V3 ( V3(..), _xy )
import "linear" Linear.V4 ( V4(..) )
import "opencv" OpenCV
import "opencv" OpenCV.Unsafe
import "opencv" OpenCV.Internal.Core.Types.Mat ( deallocateMatM, newMat )
import "opencv" OpenCV.Internal.Core.Types.Mat.Marshal ( marshalDepth, unmarshalDepth )
import qualified "repa" Data.Array.Repa as Repa
import "repa" Data.Array.Repa.Index ((:.)((:.)))
import "tasty" Test.Tasty
import "tasty-hunit" Test.Tasty.HUnit as HU
import qualified "tasty-quickcheck" Test.Tasty.QuickCheck as QC ( testProperty )
import qualified "QuickCheck" Test.QuickCheck as QC
import           "QuickCheck" Test.QuickCheck ( (==>) )
import qualified "QuickCheck" Test.QuickCheck.Property as QCProp
import "random" System.Random ( Random )
import "transformers" Control.Monad.Trans.Except
import qualified "vector" Data.Vector as V
import qualified "vector" Data.Vector.Storable as VS

main :: IO ()
main = defaultMain $ testGroup "opencv"
    [ testGroup "Calib3d"
      [ HU.testCase "calibrationMatrixValues identity" testCalibrationMatrixValues_identity
      , HU.testCase "findFundamentalMat - no points" testFindFundamentalMat_noPoints
      , HU.testCase "findFundamentalMat" testFindFundamentalMat
      , HU.testCase "computeCorrespondEpilines" testComputeCorrespondEpilines
      ]
    , testGroup "Core"
      [ testGroup "ArrayOps"
        [ HU.testCase "matAdd      different shapes" (testArrayBinOpArgDiff matAdd)
        , HU.testCase "matSubtract different shapes" (testArrayBinOpArgDiff matSubtract)
        , HU.testCase "matAbsDiff  different shapes" (testArrayBinOpArgDiff matAbsDiff)
        , HU.testCase "matMultiply different shapes" (testArrayBinOpArgDiff matMultiply)
        ]
      , testGroup "Iso"
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
          , HU.testCase "deallocate" $ testDeallocate
          , testGroup "newMat"
            [ testNewMat False (MatInfo []     Depth_8U 1) 0
            , testNewMat True  (MatInfo [0]    Depth_8U 1) 0
            , testNewMat True  (MatInfo [-1]   Depth_8U 1) 0
            , testNewMat True  (MatInfo [1,0]  Depth_8U 1) 0
            , testNewMat True  (MatInfo [1,-1] Depth_8U 1) 0
            , testNewMat False (MatInfo [1,3]  Depth_8U 1) 0
            , testNewMat False (MatInfo [3,1]  Depth_8U 1) 0
            , testNewMat True  (MatInfo [1]    Depth_8U 0) 0
            , testNewMat True  (MatInfo [1]    Depth_8U (-1)) 0
            , testNewMat False (MatInfo [1,1]  Depth_8U 512) 0
            , testNewMat True  (MatInfo [1,1]  Depth_8U 513) 0
            , testNewMat False (MatInfo (replicate  32 1) Depth_8U 1) 0
            , testNewMat True  (MatInfo (replicate 100 1) Depth_8U 1) 0
            ]
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
          , testGroup "mat <-> vector conversions"
            [ testGroup "unsafeWithMatAsVec"
              [ HU.testCase "eye_m33 Word8"  $ testUnsafeWithMatAsVec (eye_m33 :: M33 Word8)
              , HU.testCase "eye_m33 Double" $ testUnsafeWithMatAsVec (eye_m33 :: M33 Double)
              , HU.testCase "1..9 Int16" $ testUnsafeWithMatAsVec (V3 (V3 1 2 3) (V3 4 5 6) (V3 7 8 9) :: M33 Int16)
              ]
            , testGroup "mat -> vec -> mat"
              [ HU.testCase "eye_m33 Word8" $ testMatVecMat (eye_m33 :: M33 Word8)
              , HU.testCase "eye_m33 Double" $ testMatVecMat (eye_m33 :: M33 Double)
              , HU.testCase "1..9 Int16" $ testMatVecMat (V3 (V3 1 2 3) (V3 4 5 6) (V3 7 8 9) :: M33 Int16)
              ]
            , testGroup "vec -> mat -> vec"
              [ HU.testCase "eye_m33 Word8" $ testVecMatVec (eye_m33 :: M33 Word8)
              , HU.testCase "eye_m33 Double" $ testVecMatVec (eye_m33 :: M33 Double)
              , HU.testCase "1..9 Int16" $ testVecMatVec (V3 (V3 1 2 3) (V3 4 5 6) (V3 7 8 9) :: M33 Int16)
              ]
            , testGroup "unsafe mat -> vec -> mat"
              [ HU.testCase "eye_m33 Word8" $ testUnsafeMatVecMat (eye_m33 :: M33 Word8)
              , HU.testCase "eye_m33 Double" $ testUnsafeMatVecMat (eye_m33 :: M33 Double)
              , HU.testCase "1..9 Int16" $ testUnsafeMatVecMat (V3 (V3 1 2 3) (V3 4 5 6) (V3 7 8 9) :: M33 Int16)
              ]
            , testGroup "unsafe vec -> mat -> vec"
              [ HU.testCase "eye_m33 Word8" $ testUnsafeVecMatVec (eye_m33 :: M33 Word8)
              , HU.testCase "eye_m33 Double" $ testUnsafeVecMatVec (eye_m33 :: M33 Double)
              , HU.testCase "1..9 Int16" $ testUnsafeVecMatVec (V3 (V3 1 2 3) (V3 4 5 6) (V3 7 8 9) :: M33 Int16)
              ]
            ]
          ]
        ]
      ]
    , testGroup "ImgProc"
      [ testGroup "Drawing"
        [ QC.testProperty "fillConvexPoly" fillConvexPolyProp
        , QC.testProperty "fillPoly" fillPolyProp
        , QC.testProperty "polylines" polylinesProp
        ]
      , testGroup "GeometricImgTransform"
        [ HU.testCase "getRotationMatrix2D" testGetRotationMatrix2D
        ]
      , testGroup "Structural Analysis and Shape Descriptors"
        [ HU.testCase "findContours" testFindContours
        ]
      , testGroup "Feature Detection"
        [ HU.testCase "houghLinesP" testHoughLinesP
        ]
      , testGroup "Cascade Classifier"
        [ HU.testCase "newCascadeClassifier algorithm" testNewCascadeClassifierAlgorithm
        , HU.testCase "cascadeClassifierDetectMultiScale Arnold" testCascadeClassifierDetectMultiScaleArnold
        ]
      ]
    , testGroup "ImgCodecs"
      [ testGroup "imencode . imdecode"
        [ HU.testCase "OutputBmp"      $ encodeDecode ImreadUnchanged OutputBmp
-- !?!?!, HU.testCase "OutputExr"      $ encodeDecode ImreadUnchanged OutputExr
        , HU.testCase "OutputHdr"      $ encodeDecode ImreadUnchanged (OutputHdr True)
-- !?!?!, HU.testCase "OutputJpeg"     $ encodeDecode ImreadUnchanged (OutputJpeg defaultJpegParams)
        -- , HU.testCase "OutputJpeg2000" $ do
        --                                  -- See https://github.com/opencv/opencv/issues/14058
        --                                  setEnv "OPENCV_IO_ENABLE_JASPER" "1"
        --                                  encodeDecode ImreadUnchanged OutputJpeg2000
        , HU.testCase "OutputPng"      $ encodeDecode ImreadUnchanged (OutputPng defaultPngParams)
        , HU.testCase "OutputPnm"      $ encodeDecode ImreadUnchanged (OutputPnm True)
        , HU.testCase "OutputPbm"      $ encodeDecode ImreadGrayscale (OutputPbm True)
        , HU.testCase "OutputPgm"      $ encodeDecode ImreadGrayscale (OutputPgm True)
        , HU.testCase "OutputPpm"      $ encodeDecode ImreadUnchanged (OutputPpm True)
        , HU.testCase "OutputSunras"   $ encodeDecode ImreadUnchanged OutputSunras
        , HU.testCase "OutputTiff"     $ encodeDecode ImreadUnchanged OutputTiff
-- !?!?!, HU.testCase "OutputWebP"     $ encodeDecode ImreadUnchanged (OutputWebP 100)
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
    , QC.testProperty "getAffineTransform" getAffineTransformProp
    ]

testArrayBinOpArgDiff
    :: forall testShapeA testShapeB
     . ( testShapeA ~ 'S ['S 2, 'S 3]
       , testShapeB ~ 'S ['S 4, 'S 4]
       )
    => (Mat 'D 'D 'D -> Mat 'D 'D 'D -> Either CvException (Mat 'D 'D 'D))
    -> HU.Assertion
testArrayBinOpArgDiff arrayOp =
    case arrayOp (relaxMat a) (relaxMat b) of
      Left _err -> pure ()
      Right _mat -> assertFailure "result despite different shapes"
  where
    a :: Mat testShapeA ('S 1) ('S Double)
    a = exceptError $ mkMat (Proxy :: Proxy testShapeA)
                            (Proxy :: Proxy 1)
                            (Proxy :: Proxy Double)
                            black

    b :: Mat testShapeB ('S 1) ('S Double)
    b = exceptError $ mkMat (Proxy :: Proxy testShapeB)
                            (Proxy :: Proxy 1)
                            (Proxy :: Proxy Double)
                            black

testCalibrationMatrixValues_identity :: HU.Assertion
testCalibrationMatrixValues_identity = evaluate x $> ()
  where
    x = calibrationMatrixValues (toMat eye_m33) (V2 1920 1080) 15 10

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
    for_ [minBound .. maxBound] $ \depth ->
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

testDeallocate :: HU.Assertion
testDeallocate = do
    mat <- loadLambda
    mutMat <- thaw mat
    deallocateMatM mutMat

testNewMat :: Bool -> MatInfo -> V4 Double -> TestTree
testNewMat throwsException infoIn defValue =
    HU.testCase testName $ checkException $ do
        mat <- exceptErrorIO $ newMat shapeIn (miChannels infoIn) (miDepth infoIn) defValue
        let infoOut = matInfo mat
        HU.assertEqual "Mat not created according to specification" infoIn infoOut
  where
    testName = concat
        [ "shape "
        , if shapeInLen <= 4
          then show shapeIn
          else show shapeInLen ++ "D"
        , " channels ", show $ miChannels infoIn
        , " depth "   , show $ miDepth    infoIn
        ]

    shapeIn = miShape infoIn
    shapeInLen = length shapeIn

    checkException :: IO () -> Assertion
    checkException
        | throwsException = assertException acceptCvException
        | otherwise = id

    acceptCvException (CvException _) = True
    acceptCvException _ = False

matHasInfoFP :: FilePath -> MatInfo -> TestTree
matHasInfoFP fp expectedInfo = HU.testCase fp $ do
    mat <- loadImg ImreadUnchanged fp
    assertEqual "" expectedInfo (matInfo mat)

mkDrawingProp
    :: ( Mat ('S '[ 'D, 'D ]) ('S 3) ('S Word8)
       -> QC.Property
       )
    -> QC.Property
mkDrawingProp withImg =
    QC.forAll genImgSize $ \(V2 w h) ->
    QC.forAll QC.arbitrary $ \(imgColor :: V4 Double) -> do
      let img = exceptError $ mkMat (w ::: h ::: Z) (Proxy @3) (Proxy @Word8) imgColor
      withImg img
  where
    maxDimSize = 100

    genImgSize :: QC.Gen (V2 Int32)
    genImgSize =
        V2 <$> maxSizedPosRange maxDimSize
           <*> maxSizedPosRange maxDimSize

fillConvexPolyProp :: V4 Double -> LineType -> QC.Property
fillConvexPolyProp drawColor lineType =
    mkDrawingProp $ \img ->
    QC.forAll genPoints $ \points -> QC.ioProperty $ do
      imgM <- unsafeThaw img
      exceptErrorIO $ fillConvexPoly imgM (V.fromList points) drawColor lineType 0
  where
    maxNumPolyPts = 10
    maxCoordVal   = 20

    genPoints :: QC.Gen [V2 Int32]
    genPoints = do
        n <- maxSizedNonNegRange maxNumPolyPts
        QC.vectorOf n $
          V2 <$> maxSizedRange maxCoordVal <*> maxSizedRange maxCoordVal

fillPolyProp :: V4 Double -> LineType -> QC.Property
fillPolyProp drawColor lineType =
    mkDrawingProp $ \img ->
    QC.forAll genPolysPoints $ \polysPts -> QC.ioProperty $ do
      imgM <- unsafeThaw img
      let vs :: V.Vector (V.Vector (V2 Int32))
          vs = V.fromList $ map V.fromList polysPts
      exceptErrorIO $ fillPoly imgM vs drawColor lineType 0
  where
    maxNumPolys   = 5
    maxNumPolyPts = 10
    maxCoordVal   = 20

    genPolysPoints :: QC.Gen [[V2 Int32]]
    genPolysPoints = do
        numPolys <- maxSized maxNumPolys QC.arbitrary
        numPts <- maxSized maxNumPolyPts QC.arbitrary
        QC.vectorOf numPolys $ QC.vectorOf numPts $
          V2 <$> maxSizedRange maxCoordVal <*> maxSizedRange maxCoordVal

polylinesProp :: Bool -> V4 Double -> LineType -> QC.Property
polylinesProp closed drawColor lineType =
    mkDrawingProp $ \img ->
    QC.forAll (maxSizedPosRange maxEdgeThickness) $ \edgeThickness ->
    QC.forAll genPolysPoints $ \polysPts -> QC.ioProperty $ do
      imgM <- unsafeThaw img
      let vs :: V.Vector (V.Vector (V2 Int32))
          vs = V.fromList $ map V.fromList polysPts
      exceptErrorIO $ polylines imgM vs closed drawColor edgeThickness lineType 0
  where
    maxEdgeThickness = 20
    maxNumPolys      = 5
    maxNumPolyPts    = 10
    maxCoordVal      = 20

    genPolysPoints :: QC.Gen [[V2 Int32]]
    genPolysPoints = do
        numPolys <- maxSizedRange maxNumPolys
        numPts <- maxSizedRange maxNumPolyPts
        QC.vectorOf numPolys $ QC.vectorOf numPts $
          V2 <$> maxSizedRange maxCoordVal <*> maxSizedRange maxCoordVal

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

encodeDecode :: ImreadMode -> OutputFormat -> HU.Assertion
encodeDecode imreadMode outputFormat = do
    mat1 <- loadImg imreadMode "Lenna.png"

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
    lineSegments <- exceptErrorIO $ houghLinesP 1 (pi / 180) 100 Nothing Nothing edgeImgM
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
    arnoldEyes <- exceptErrorIO $
      cascadeClassifierDetectMultiScale
        ccEyes
        Nothing
        Nothing
        (Nothing :: Maybe (V2 Int32))
        (Nothing :: Maybe (V2 Int32))
        arnoldGray
    assertBool "unexpected number of eyes detected" (V.length arnoldEyes == 3)
    arnoldFront <- exceptErrorIO $
      cascadeClassifierDetectMultiScale
        ccFrontal
        Nothing
        Nothing
        (Nothing :: Maybe (V2 Int32))
        (Nothing :: Maybe (V2 Int32))
        arnoldGray
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

testUnsafeWithMatAsVec :: (ToMat (M33 a), Storable a, Eq a, Show a, NFData a) => M33 a -> HU.Assertion
testUnsafeWithMatAsVec m33 = do
    xs <- unsafeWithMatAsVec (toMat m33) $ \vec ->
      evaluate $ force $ VS.toList vec
    assertEqual "" (concatMap toList $ toList m33) xs

testMatVecMat
    :: forall a
     . (Eq a, Show a, ToDepth (Proxy a), Storable a, ToMat (M33 a))
    => M33 a -> HU.Assertion
testMatVecMat m33In = assertEqual "" m33In m33Out
  where
    m33Out :: M33 a
    m33Out = fromMat $ exceptError $ vecToMat (Proxy @[3, 3]) (Proxy @1) $ matToVec $ toMat m33In

testVecMatVec
    :: forall a
     . (Eq a, Show a, NFData a, ToDepth (Proxy a), Storable a)
    => M33 a -> HU.Assertion
testVecMatVec m33In = assertEqual "" elemsIn elemsOut
  where
    elemsOut :: [a]
    elemsOut = VS.toList $ matToVec $ exceptError $ vecToMat (Proxy @[3, 3]) (Proxy @1) vecIn

    vecIn :: VS.Vector a
    vecIn = VS.fromList elemsIn

    elemsIn :: [a]
    elemsIn = concatMap toList $ toList m33In

testUnsafeMatVecMat
    :: forall a
     . (Eq a, Show a, NFData a, ToDepth (Proxy a), Storable a, ToMat (M33 a))
    => M33 a -> HU.Assertion
testUnsafeMatVecMat m33In = do
    m33Out <-
      unsafeWithMatAsVec matIn $ \(vec :: VS.Vector a) ->
        exceptErrorIO $
          unsafeWithVecAsMat (Proxy @[3, 3]) (Proxy @1) vec $ \matOut ->
            liftIO $ evaluate $ fromMat matOut
    assertEqual "" m33In m33Out
  where
    matIn :: Mat ('S '[ 'S 3, 'S 3 ]) ('S 1) ('S a)
    matIn = toMat m33In

testUnsafeVecMatVec
    :: forall a
     . (Eq a, Show a, NFData a, ToDepth (Proxy a), Storable a)
    => M33 a -> HU.Assertion
testUnsafeVecMatVec m33In = do
    elemsOut <- exceptErrorIO $
      unsafeWithVecAsMat (Proxy @[3, 3]) (Proxy @1) vecIn $ \mat ->
        liftIO $ unsafeWithMatAsVec mat $ \vecOut ->
          evaluate $ force $ VS.toList vecOut
    assertEqual "" elemsIn elemsOut
  where
    vecIn :: VS.Vector a
    vecIn = VS.fromList elemsIn

    elemsIn :: [a]
    elemsIn = concatMap toList $ toList m33In

getAffineTransformProp :: V3 (V2 CFloat) -> V3 (V2 CFloat) -> QCProp.Result
getAffineTransformProp v v' =
    let transfEither = getAffineTransform v v'
    in case transfEither of
        Left exception -> failedWithReason $ displayException exception
        Right transf ->
            let m23 = fmap realToFrac <$> fromMat transf
                newPoints = warpAffineV2 m23 <$> v
            in case nonEmpty $ typeCheckMat transf of
                Nothing ->
                    if and $ almostEqualFloats <$> v' <*> newPoints
                        then QCProp.succeeded
                        else failedWithReason $ "The points are too far apart; newPoints =\n" <> show newPoints <> "\n"
                Just errs -> failedWithReason . displayException $ CoerceMatError errs

warpAffineV2 :: (Num a) => M23 a -> V2 a -> V2 a
warpAffineV2 m23 pt = view _xy (m23 !* homogeneous pt)
  where
    homogeneous :: (Num a) => V2 a -> V3 a
    homogeneous (V2 x y) = V3 x y 1

failedWithReason :: String -> QCProp.Result
failedWithReason s = QCProp.failed { QCProp.reason = s }

almostEqualFloats :: V2 CFloat -> V2 CFloat -> Bool
almostEqualFloats v v' = isSmall $ relError v v'

relError :: V2 CFloat -> V2 CFloat -> CFloat
relError x y
    | isSmall (L.norm x) = L.distance x y
    | otherwise = L.distance x y / L.norm x

isSmall :: CFloat -> Bool
isSmall = (>) 2e-2 . abs

--------------------------------------------------------------------------------

eye23_8u_1c :: Mat (ShapeT [2, 3]) ('S 1) ('S Word8)
eye33_8u_1c :: Mat (ShapeT [3, 3]) ('S 1) ('S Word8)
eye22_8u_3c :: Mat (ShapeT [2, 2]) ('S 3) ('S Word8)

eye23_8u_1c = exceptError $ eyeMat (Proxy @2) (Proxy @3) (Proxy @1) (Proxy @Word8)
eye33_8u_1c = exceptError $ eyeMat (Proxy @3) (Proxy @3) (Proxy @1) (Proxy @Word8)
eye22_8u_3c = exceptError $ eyeMat (Proxy @2) (Proxy @2) (Proxy @3) (Proxy @Word8)

eye_m23 :: (Num e) => M23 e
eye_m33 :: (Num e) => M33 e

eye_m23 = V2 (V3 1 0 0) (V3 0 1 0)
eye_m33 = V3 (V3 1 0 0) (V3 0 1 0) (V3 0 0 1)

black :: V4 Double
black = 0

--------------------------------------------------------------------------------
-- Generators
--------------------------------------------------------------------------------

-- Modifies a generator so that the size is never greater than maxSize.
maxSized :: Int -> QC.Gen a -> QC.Gen a
maxSized maxSize = QC.scale (min maxSize)

maxSizedRangeHelper :: (Random a, Num a) => (a -> (a, a)) -> Int -> QC.Gen a
maxSizedRangeHelper mkBounds maxSize =
    maxSized maxSize $ QC.sized $ \n ->
        let n' = fromIntegral n
        in QC.choose $ mkBounds n'

maxSizedRange :: (Random a, Num a) => Int -> QC.Gen a
maxSizedRange = maxSizedRangeHelper $ \n -> (-n, n)

maxSizedNonNegRange :: (Random a, Ord a, Num a) => Int -> QC.Gen a
maxSizedNonNegRange = maxSizedRangeHelper $ \n -> (0, n)

maxSizedPosRange :: (Random a, Ord a, Num a) => Int -> QC.Gen a
maxSizedPosRange = maxSizedRangeHelper $ \n -> (1, max 1 n)

--------------------------------------------------------------------------------
-- QuickCheck Arbitrary Instances
--------------------------------------------------------------------------------

#if !MIN_VERSION_QuickCheck(2,10,1)
instance QC.Arbitrary CFloat where
    arbitrary = CFloat <$> QC.arbitrary

instance QC.Arbitrary CDouble where
    arbitrary = CDouble <$> QC.arbitrary
#endif

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

instance QC.Arbitrary LineType where
    arbitrary = QC.elements [LineType_8, LineType_4, LineType_AA]

--------------------------------------------------------------------------------

assertNull
    :: [a] -- ^ List that should be empty.
    -> ([a] -> String) -- ^ Error when the list is not empty.
    -> IO ()
assertNull [] _showError = pure ()
assertNull xs  showError = assertFailure $ showError xs

-- | Asserts that an exception is thrown.
assertException
    :: (Exception e)
    => (e -> Bool) -- | Function which accepts or rejects the exception.
    -> IO () -- | Action which should throw an exception.
    -> Assertion
assertException acceptException act =
    try act >>= \case
      Left e -> assertBool ("Unexpected exception: " <> displayException e) $ acceptException e
      Right _ -> HU.assertFailure "Excepted exception was not thrown"
