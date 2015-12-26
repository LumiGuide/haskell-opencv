{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import           "base"                  Foreign.Storable (Storable(..))
import           "base"                  Foreign.Ptr (castPtr)
import           "lumi-hackage-extended" Lumi.Prelude
import qualified "bytestring"            Data.ByteString as B
import           "lens"                  Control.Lens
import           "linear"                Linear.Matrix ( M23, M33 )
import           "linear"                Linear.Vector ( (^+^), zero  )
import           "linear"                Linear.V2 ( V2(..) )
import           "linear"                Linear.V3 ( V3(..) )
import           "linear"                Linear.V4 ( V4(..) )
import qualified "repa"                  Data.Array.Repa as Repa
import           "repa"                  Data.Array.Repa.Index (Z(Z),(:.)((:.)))
import           "tasty"                 Test.Tasty
import           "tasty-hunit"           Test.Tasty.HUnit      as HU
import qualified "tasty-quickcheck"      Test.Tasty.QuickCheck as QC (testProperty)
import qualified "QuickCheck"            Test.QuickCheck       as QC
import           "thea"                  OpenCV
import qualified "vector"                Data.Vector as V
-- import qualified "vector"                Data.Vector.Unboxed as VU

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
          [ HU.testCase "emptyToRepa" emptyToRepa
          , HU.testCase "imgToRepa"   imgToRepa
          , testGroup "matToRepa"
            [ --        Size     Elem type    Channels Elem value Repa dimensions            Repa elem type          Expected elem at index
              matToRepa [2,3]    MatDepth_8U  1        zero       (Proxy :: Proxy Repa.DIM2) (Proxy :: Proxy Word8)  (Just (Z :. 0 :. 1,      0))
            , matToRepa []       MatDepth_8U  1        zero       (Proxy :: Proxy Repa.DIM0) (Proxy :: Proxy NoElem) Nothing
            , matToRepa [2,3,10] MatDepth_64F 1        zero       (Proxy :: Proxy Repa.DIM3) (Proxy :: Proxy Double) (Just (Z :. 2 :. 1 :. 0, 0))
    -- !?!?!, matToRepa [3]      MatDepth_8U  1        zero       (Proxy :: Proxy Repa.DIM1) (Proxy :: Proxy Word8)  (Just (Z :. 2,           0))
            ]
          ]
        , testGroup "fixed size matrices"
          [ HU.testCase "M23 eye" $ testMatToM23 eye23_8u_1c (eye_m23 :: M23 Word8)
          , HU.testCase "M33 eye" $ testMatToM33 eye33_8u_1c (eye_m33 :: M33 Word8)
          ]
        ]
      ]
    , testGroup "ImgProc"
      [
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


testIso :: (QC.Arbitrary b, Eq b, Show b) => String -> Iso' a b -> TestTree
testIso name myIso = QC.testProperty name $ \b -> b QC.=== (b ^. from myIso . myIso)

rectBasicProperties
    :: V2 Int -- ^ tl
    -> V2 Int -- ^ size
    -> Bool
rectBasicProperties tl size@(V2 w h) = and
      [ (rectTopLeft     rect ^. isoPoint2iV2) == tl
      , (rectBottomRight rect ^. isoPoint2iV2) == tl ^+^ size
      , (rectSize        rect ^. isoSize2iV2)  == size
      ,  rectArea        rect                  == (w  *  h)
      ]
    where
      rect = mkRect tl size

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

matHasInfoFP :: FilePath -> MatInfo -> TestTree
matHasInfoFP fp expectedInfo = HU.testCase fp $ do
    mat <- loadImg ImreadUnchanged fp
    assertEqual "" expectedInfo (matInfo mat)

hMatEncodeDecodeFP :: FilePath -> TestTree
hMatEncodeDecodeFP fp = HU.testCase fp $ do
    mat <- loadImg ImreadUnchanged fp
    hMatEncodeDecode mat

hMatEncodeDecode :: Mat -> HU.Assertion
hMatEncodeDecode m1 =
    assertEqual "" h1 h2
    -- assertBool "mat hmat conversion failure" (h1 == h2)
  where
    h1 = m1 ^. hmat
    m2 = h1 ^. from hmat
    h2 = m2 ^. hmat

hmatElemSize :: FilePath -> Int -> HU.Assertion
hmatElemSize fp expectedElemSize = do
  mat <- loadImg ImreadUnchanged fp
  assertEqual "" expectedElemSize $ hElemsLength $ hmElems $ mat ^. hmat

encodeDecode :: OutputFormat -> HU.Assertion
encodeDecode outputFormat = do
    mat1 <- loadImg ImreadUnchanged "Lenna.png"

    let bs2  = either throw id $ imencode outputFormat mat1
        mat2 = imdecode ImreadUnchanged bs2

        bs3  = either throw id $ imencode outputFormat mat2

    assertBool "imencode . imdecode failure"
               (bs2 == bs3)

loadImg :: ImreadMode -> FilePath -> IO Mat
loadImg readMode fp = imdecode readMode <$> B.readFile ("data/" <> fp)

emptyToRepa :: HU.Assertion
emptyToRepa = do
    emptyMat <- newEmptyMat
    assertBool "Repa conversion failure" $
      isJust (emptyMat ^? repa :: Maybe (Repa.Array M Repa.DIM0 NoElem))

data NoElem = NoElem deriving (Show, Eq)

instance Storable NoElem where
    sizeOf    _ = 0
    alignment _ = 0
    peek        = undefined
    poke        = undefined

imgToRepa :: HU.Assertion
imgToRepa = do
    mat <- loadImg ImreadUnchanged "kikker.jpg"
    case mat ^? repa :: Maybe (Repa.Array M Repa.DIM2 BGRElem) of
      Nothing -> assertFailure "Repa conversion failure"
      Just repaArray -> do
        assertEqual "extent" (Z :. 500 :. 390) (Repa.extent repaArray)

data BGRElem = BGRElem Word8 Word8 Word8

instance Storable BGRElem where
    sizeOf    _ = 3
    alignment _ = 0
    peek ptr = BGRElem <$> peekElemOff (castPtr ptr) 0
                       <*> peekElemOff (castPtr ptr) 1
                       <*> peekElemOff (castPtr ptr) 2
    poke ptr (BGRElem b g r) = do
      pokeElemOff (castPtr ptr) 0 b
      pokeElemOff (castPtr ptr) 1 g
      pokeElemOff (castPtr ptr) 2 r

matToRepa
    :: forall sh e
     . ( Typeable sh, Show sh, Repa.Shape sh
       , Typeable e,  Show e, Storable e, Eq e
       )
    => [Int]         -- ^ Sizes
    -> MatDepth
    -> Int           -- ^ Number of channels
    -> V4 Double     -- ^ Default element value
    -> Proxy sh
    -> Proxy e
    -> Maybe (sh, e) -- ^ Optional index and expected value at that index
    -> TestTree
matToRepa sz depth numChannels defValue shapeProxy elemProxy mbIndex = HU.testCase name $ do
    mat <- newMat (V.fromList sz) depth numChannels (defValue ^. from isoScalarV4)
    assertEqual "info" (MatInfo sz depth numChannels) $ matInfo mat
    case toRepa mat :: Either String (Repa.Array M sh e) of
      Left err -> assertFailure $ "Repa conversion failure: " <> err
      Right a -> do
        let size :: sh
            size = Repa.extent a

        assertEqual "size" sh size

        forM_ mbIndex $ \(i, expected) -> do
          assertEqual "index"  expected (Repa.unsafeIndex a i)

          let outOfRange :: sh
              outOfRange = Repa.shapeOfList $ zipWith (+) (Repa.listOfShape i) sz

          r <- try (evaluate (Repa.index a outOfRange))
          case r of
            Left (_err :: ErrorCall) -> pure ()
            Right _x -> assertFailure $ "Indexing on " <> show outOfRange <> " should throw an error."
  where
    sh :: sh
    sh = Repa.shapeOfList sz

    name :: String
    name = show
           ( sz
           , depth
           , numChannels
           , defValue
           , typeRep shapeProxy
           , typeRep elemProxy
           , mbIndex
           )

testMatToM23 :: (Storable e, Eq e, Show e) => Mat -> V2 (V3 e) -> HU.Assertion
testMatToM23 m v = assertEqual "" (Right v) $ matToM23 m

testMatToM33 :: (Storable e, Eq e, Show e) => Mat -> V3 (V3 e) -> HU.Assertion
testMatToM33 m v = assertEqual "" (Right v) $ matToM33 m

--------------------------------------------------------------------------------

eye23_8u_1c :: Mat
eye33_8u_1c :: Mat
eye22_8u_3c :: Mat

eye23_8u_1c = eyeMat 2 3 MatDepth_8U 1
eye33_8u_1c = eyeMat 3 3 MatDepth_8U 1
eye22_8u_3c = eyeMat 2 2 MatDepth_8U 3

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
    arbitrary = view (from isoPoint2iV2) <$> (QC.arbitrary :: QC.Gen (V2 Int))
