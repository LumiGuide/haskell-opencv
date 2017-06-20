{-# LANGUAGE DataKinds, QuasiQuotes, RecordWildCards, TemplateHaskell #-}

module OpenCV.Extra.ArUco
  ( -- * ArUco markers
    -- ** Dictionaries
    Dictionary
  , PredefinedDictionaryName(..)
  , getPredefinedDictionary

    -- ** Detecting markers
  , detectMarkers
  , ArUcoMarkers

    -- ** Visualising ArUco markers
  , drawDetectedMarkers

    -- * ChArUco markers
  , ChArUcoBoard
  , createChArUcoBoard
  , drawChArUcoBoard

    -- ** Detecting markers
  , interpolateChArUcoMarkers
  , estimatePoseChArUcoBoard

    -- ** Camera calibration
  , calibrateCameraFromFrames

    -- ** Debugging and visualiation utilities
  , drawDetectedCornersCharuco
  , drawEstimatedPose
  ) where

import "opencv" OpenCV.Internal.Exception
import "base" Control.Monad (guard)
import "primitive" Control.Monad.Primitive
import "base" Data.Int ( Int32 )
import "base" Data.Monoid ((<>))
import "base" Data.Word ( Word8 )
import qualified "vector" Data.Vector.Storable as SV
import "base" Foreign.C
import "base" Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import "base" Foreign.Marshal.Alloc
import "base" Foreign.Ptr
import "base" Foreign.Storable (peek)
import qualified "inline-c" Language.C.Inline as C
import qualified "inline-c-cpp" Language.C.Inline.Cpp as C
import qualified "inline-c" Language.C.Inline.Unsafe as CU
import "linear" Linear
import "opencv" OpenCV
import "opencv" OpenCV.Core.Types.Vec (Vec3d)
import "this" OpenCV.Extra.Internal.C.Inline ( openCvExtraCtx )
import "this" OpenCV.Extra.Internal.C.Types
import "opencv" OpenCV.Internal
import "opencv" OpenCV.Internal.C.Types
import "opencv" OpenCV.Internal.Core.Types.Mat
import "base" System.IO.Unsafe

--------------------------------------------------------------------------------
C.context openCvExtraCtx

C.include "opencv2/aruco.hpp"
C.include "opencv2/aruco/charuco.hpp"
C.include "opencv2/core.hpp"
C.include "iostream"
C.include "aruco.hpp"

C.using "namespace cv"
C.using "namespace cv::aruco"
C.using "namespace std"

#include <bindings.dsl.h>
#include "opencv2/aruco.hpp"
#include "aruco-namespace.hpp"

{-| A @Dictionary@ describes the possible QR codes used for ArUco markers. Use
'getPredefinedDictionary' to lookup known dictionaries.
-}
newtype Dictionary = Dictionary { unDictionary :: ForeignPtr C'Ptr'Dictionary }

type instance C Dictionary = C'Ptr'Dictionary

instance FromPtr Dictionary where
  fromPtr = objFromPtr Dictionary $ \ptr ->
              [CU.block| void { delete $(Ptr_Dictionary * ptr); }|]

instance WithPtr Dictionary where
  withPtr = withForeignPtr . unDictionary

{-| A ChArUco board is used to perform camera calibration from ArUco markers
overlaid on a chess board of known size. Use 'createChArUcoBoard' to create
values of this type.
-}
newtype ChArUcoBoard =
        ChArUcoBoard { unChArUcoBoard :: ForeignPtr C'Ptr'CharucoBoard }

type instance C ChArUcoBoard = C'Ptr'CharucoBoard

instance FromPtr ChArUcoBoard where
  fromPtr = objFromPtr ChArUcoBoard $ \ptr ->
              [CU.block| void { delete $(Ptr_CharucoBoard * ptr); }|]

instance WithPtr ChArUcoBoard where
  withPtr = withForeignPtr . unChArUcoBoard

newtype Vector'Int =
        Vector'Int { unVectorInt :: ForeignPtr C'Vector'Int }

type instance C Vector'Int = C'Vector'Int

instance FromPtr Vector'Int where
  fromPtr = objFromPtr Vector'Int $ \ptr ->
              [CU.block| void { delete $(VectorInt * ptr); }|]

instance WithPtr Vector'Int where
  withPtr = withForeignPtr . unVectorInt

newtype Vector'Vector'Point2f =
        Vector'Vector'Point2f { unVectorVectorPoint2f :: ForeignPtr C'Vector'Vector'Point2f }

type instance C Vector'Vector'Point2f = C'Vector'Vector'Point2f

instance FromPtr Vector'Vector'Point2f where
  fromPtr = objFromPtr Vector'Vector'Point2f $ \ptr ->
              [CU.block| void { delete $(VectorVectorPoint2f * ptr); }|]

instance WithPtr Vector'Vector'Point2f where
  withPtr = withForeignPtr . unVectorVectorPoint2f

{-| An encoding of the result of 'interpolateChArUcoMarkers'.
-}
data ChArUcoMarkers = ChArUcoMarkers
    { charucoIds :: Mat 'D 'D 'D
    , charucoCorners :: Mat 'D 'D 'D
    }

{-| Given an image and the detected ArUco markers in that image, attempt to
perform ChAruco calibration.
-}
interpolateChArUcoMarkers
  :: ChArUcoBoard
     -- ^ The ChArUco board to interpolate markers for.
  -> Mat ('S '[ h, w]) channels depth
     -- ^ A view of a ChArUco board.
  -> ArUcoMarkers
     -- ^ The ArUco markers detected in the same image.
  -> Maybe ChArUcoMarkers
interpolateChArUcoMarkers charucoBoard image ArUcoMarkers {..} =
  unsafePerformIO $
  alloca $ \charucoCornersPtr ->
  alloca $ \charucoIdsPtr ->
  withPtr arucoCorners $ \c'arucoCorners ->
  withPtr arucoIds $ \c'arucoIds ->
  withPtr image $ \imagePtr ->
  withPtr charucoBoard $ \c'charucoBoard -> do
    success <-
      [C.block| bool {
        auto & corners = *$(VectorVectorPoint2f * c'arucoCorners);
        auto & ids = *$(VectorInt * c'arucoIds);
        auto & frame = *$(Mat * imagePtr);

        auto charucoCorners = new Mat();
        auto charucoIds = new Mat();

        interpolateCornersCharuco(corners,
                                  ids,
                                  frame,
                                  *$(Ptr_CharucoBoard * c'charucoBoard),
                                  *charucoCorners,
                                  *charucoIds);

        *$(Mat * * charucoIdsPtr) = charucoIds;
        *$(Mat * * charucoCornersPtr) = charucoCorners;

        return !charucoIds->empty();
      }|]
    ids <- fromPtr (peek charucoIdsPtr)
    corners <- fromPtr (peek charucoCornersPtr)
    return (ChArUcoMarkers ids corners <$ guard (success /= 0))

{- | Given an image, the ChArUco markers in that image, and the camera
calibration, estimate the pose of the board.
-}
estimatePoseChArUcoBoard
  :: ChArUcoBoard
     -- ^ The ChArUco board parameters.
  -> ChArUcoMarkers
     -- ^ Detected ChArUco markers.
  -> (Matx33d, Matx51d)
     -- ^ A pair of the camera intrinsic parameters and a 5 dimensional vector
     -- of distortion coefficients.
  -> Maybe (Vec3d, Vec3d)
estimatePoseChArUcoBoard charucoBoard ChArUcoMarkers {..} (cameraMatrix, distCoeffs) =
  unsafePerformIO $ do
    rvec <- toVecIO (V3 0.0 0.0 0.0)
    tvec <- toVecIO (V3 0.0 0.0 0.0)
    withPtr cameraMatrix $ \c'cameraMatrix ->
      withPtr distCoeffs $ \c'distCoeffs ->
      withPtr charucoIds $ \c'idsPtr ->
      withPtr charucoBoard $ \c'charucoBoard ->
      withPtr rvec $ \rvecPtr ->
      withPtr tvec $ \tvecPtr ->
      withPtr charucoCorners $ \c'cornersPtr -> do
        success <- [C.block| bool {
          return estimatePoseCharucoBoard(*$(Mat * c'cornersPtr),
                                          *$(Mat * c'idsPtr),
                                          *$(Ptr_CharucoBoard * c'charucoBoard),
                                          *$(Matx33d * c'cameraMatrix),
                                          *$(Matx51d * c'distCoeffs),
                                          *$(Vec3d * rvecPtr),
                                          *$(Vec3d * tvecPtr));
          }|]
        return (( fromVec rvec , fromVec tvec) <$ guard (success /= 0))

{- | Given an estimated pose for a board, draw the axis over an image.
-}
drawEstimatedPose
  :: PrimMonad m
  => Matx33d
     -- ^ The matrix of intrinsic parameters of a camera.
  -> Matx51d
     -- ^ A 5-dimensional vector of distortion coefficients.
  -> (Vec3d, Vec3d)
     -- ^ The transposition and rotation matrices from local to camera space,
     -- respectively.
  -> Mut (Mat ('S '[ h, w]) channels depth) (PrimState m)
     -- ^ An image to draw the axis onto.
  -> m ()
drawEstimatedPose cameraMatrix distCoeffs (rvec, tvec) image =
  unsafePrimToPrim $ do
    withPtr image $ \imagePtr ->
      withPtr cameraMatrix $ \c'cameraMatrix ->
      withPtr distCoeffs $ \c'distCoeffs ->
      withPtr rvec $ \rvecPtr ->
      withPtr tvec $ \tvecPtr ->
      [C.block| void {
        drawAxis(*$(Mat * imagePtr),
                 *$(Matx33d * c'cameraMatrix),
                 *$(Matx51d * c'distCoeffs),
                 *$(Vec3d * rvecPtr),
                 *$(Vec3d * tvecPtr),
                 1);
      }|]

{- | Given a list of ChArUco calibration results, combine all results into
camera calibration.
-}
calibrateCameraFromFrames
    :: ChArUcoBoard
    -> Int
    -> Int
    -> [(ArUcoMarkers, ChArUcoMarkers)]
    -> CvExcept (Matx33d, Matx51d)
calibrateCameraFromFrames board width height frames =
  unsafeWrapException $ do
    cameraMatrix <- newMatx33d 0 0 0 0 0 0 0 0 0
    distCoeffs <- newMatx51d 0 0 0 0 0
    handleCvException (pure (cameraMatrix, distCoeffs)) $
      withPtr cameraMatrix $ \cameraMatrixPtr ->
      withPtr distCoeffs $ \distCoeffsPtr ->
      withPtr board $ \c'board ->
      withPtrs (map (arucoIds . fst) frames) $ \c'allIds ->
      withPtrs (map (arucoCorners . fst) frames) $ \c'allCorners ->
      withPtrs (fmap (charucoCorners . snd) frames) $ \c'allCharucoCorners ->
      withPtrs (fmap (charucoIds . snd) frames) $ \c'allCharucoIds -> do

        [cvExcept|
          vector< vector<Point2f> > allCorners;
          for(auto i = 0; i < $vec-len:c'allCorners; i++) {
            auto & corners =
              *$vec-ptr:(VectorVectorPoint2f * * c'allCorners)[i];

            allCorners.insert(allCorners.end(), corners.begin(), corners.end());
          }

          vector<int> allIds;
          vector<int> counter;
          for(auto i = 0; i < $vec-len:c'allIds; i++) {
            auto & ids = *$vec-ptr:(VectorInt * * c'allIds)[i];
            allIds.insert(allIds.end(), ids.begin(), ids.end());
            counter.push_back(ids.size());
          }

          Size frameSize($(int c'width), $(int c'height));

          Ptr<CharucoBoard> charucoBoard = *$(Ptr_CharucoBoard * c'board);
          Ptr<cv::aruco::Board> board = charucoBoard.staticCast<cv::aruco::Board>();

          calibrateCameraAruco(allCorners,
                              allIds,
                              counter,
                              board,
                              frameSize,
                              *$(Matx33d * cameraMatrixPtr),
                              *$(Matx51d * distCoeffsPtr));

          vector<Mat> allCharucoCorners;
          for(auto i = 0; i < $vec-len:c'allCharucoCorners; i++) {
            auto & corners = *$vec-ptr:(Mat * * c'allCharucoCorners)[i];
            allCharucoCorners.push_back(corners);
          }

          vector<Mat> allCharucoIds;
          for(auto i = 0; i < $vec-len:c'allCharucoIds; i++) {
            auto & ids = *$vec-ptr:(Mat * * c'allCharucoIds)[i];
            allCharucoIds.push_back(ids);
          }

          Mat perViewErrors;

          calibrateCameraCharuco(allCharucoCorners,
                                allCharucoIds,
                                charucoBoard,
                                frameSize,
                                *$(Matx33d * cameraMatrixPtr),
                                *$(Matx51d * distCoeffsPtr),
                                noArray(),
                                noArray(),
                                noArray(),
                                noArray(),
                                perViewErrors);
        |]
  where
    c'width = fromIntegral width
    c'height = fromIntegral height

{- | The result of calling 'detectMarkers' on an image.
-}
data ArUcoMarkers = ArUcoMarkers
    { arucoCorners :: Vector'Vector'Point2f
    , arucoIds :: Vector'Int
    }

{- | Perform ArUco marker detection.
-}
detectMarkers
  :: Dictionary
     -- ^ A dictionary describing ArUco markers.
  -> Mat ('S '[ h, w]) channels depth
     -- ^ The matrix to detect markers from.
  -> Maybe ArUcoMarkers
detectMarkers dictionary image =
  unsafePerformIO $
  withPtr image $ \imagePtr ->
  withPtr dictionary $ \c'dictionary ->
  alloca $ \cornersOutPtr ->
  alloca $ \idsOutPtr -> do
    success <- fmap (/= 0) $
      [C.block| bool {
        auto * corners = new vector< vector<Point2f> >();
        auto * ids = new vector<int>();

        detectMarkers(*$(Mat * imagePtr),
                      *$(Ptr_Dictionary * c'dictionary),
                      *corners,
                      *ids);

        *$(VectorVectorPoint2f * * cornersOutPtr) = corners;
        *$(VectorInt * * idsOutPtr) = ids;
        return ids->size() > 0;
      }|]
    corners <- fromPtr (peek cornersOutPtr)
    ids <- fromPtr (peek idsOutPtr)
    return (ArUcoMarkers corners ids <$ guard success)

{- | Given a frame, overlay the result of ArUco marker detection.
-}
drawDetectedMarkers
  :: PrimMonad m
  => Mut (Mat ('S [h, w]) channels depth) (PrimState m)
    -- ^ The image to draw detected markers onto. Usually the same image you
    -- detected markers from.
  -> ArUcoMarkers
    -- ^ The ArUco markers to draw.
  -> m ()
drawDetectedMarkers image ArUcoMarkers{..} =
  unsafePrimToPrim $
  withPtr image $ \imagePtr ->
  withPtr arucoCorners $ \c'cornersPtr ->
  withPtr arucoIds $ \c'idsPtr ->
  [C.block| void {
    drawDetectedMarkers(*$(Mat * imagePtr),
                        *$(VectorVectorPoint2f * c'cornersPtr),
                        *$(VectorInt * c'idsPtr));
  }|]

{- | Given a frame, overlay the result of ChArUco marker detection.
-}
drawDetectedCornersCharuco
  :: PrimMonad m
  => Mut (Mat ('S '[ h, w]) channels depth) (PrimState m)
    -- ^ The image to draw detected corners.
  -> ChArUcoMarkers
    -- ^ The ChArUco markers corners to draw.
  -> m ()
drawDetectedCornersCharuco image ChArUcoMarkers{..} =
  unsafePrimToPrim $
  withPtr image $ \imagePtr ->
  withPtr charucoIds $ \c'idsPtr ->
  withPtr charucoCorners $ \c'cornersPtr ->
  [C.block| void {
    drawDetectedCornersCharuco(*$(Mat * imagePtr),
                               *$(Mat * c'cornersPtr),
                               *$(Mat * c'idsPtr));
  }|]

{-| Create a new ChArUco board configuration.
-}
createChArUcoBoard
  :: Int
    -- ^ The amount of squares along the X-axis.
  -> Int
    -- ^ The amount of squares along the Y-axis.
  -> Double
    -- ^ The length of a side of a chess-board square.
  -> Double
    -- ^ The length of a marker's side within a chess-board square.
  -> Dictionary
    -- ^ The dictionary of ArUco markers.
  -> ChArUcoBoard
createChArUcoBoard squaresX squaresY squareLength markerLength dictionary =
  unsafePerformIO $
  withPtr dictionary $ \c'dictionary ->
  fromPtr $
  [C.block| Ptr_CharucoBoard * {
    return
      new Ptr<CharucoBoard>(CharucoBoard::create($(int c'squaresX),
                                                  $(int c'squaresY),
                                                  $(double c'squareLength),
                                                  $(double c'markerLength),
                                                  *$(Ptr_Dictionary * c'dictionary)));
  }|]
  where c'squaresX = fromIntegral squaresX
        c'squaresY = fromIntegral squaresY
        c'squareLength = realToFrac squareLength
        c'markerLength = realToFrac markerLength

{-| The set of predefined ArUco dictionaries known to OpenCV.
-}
data PredefinedDictionaryName
    = DICT_4X4_50
    | DICT_4X4_100
    | DICT_4X4_250
    | DICT_4X4_1000
    | DICT_5X5_50
    | DICT_5X5_100
    | DICT_5X5_250
    | DICT_5X5_1000
    | DICT_6X6_50
    | DICT_6X6_100
    | DICT_6X6_250
    | DICT_6X6_1000
    | DICT_7X7_50
    | DICT_7X7_100
    | DICT_7X7_250
    | DICT_7X7_1000
    | DICT_ARUCO_ORIGINAL
      deriving (Show, Eq)

#num DICT_4X4_50
#num DICT_4X4_100
#num DICT_4X4_250
#num DICT_4X4_1000
#num DICT_5X5_50
#num DICT_5X5_100
#num DICT_5X5_250
#num DICT_5X5_1000
#num DICT_6X6_50
#num DICT_6X6_100
#num DICT_6X6_250
#num DICT_6X6_1000
#num DICT_7X7_50
#num DICT_7X7_100
#num DICT_7X7_250
#num DICT_7X7_1000
#num DICT_ARUCO_ORIGINAL

marshalPredefinedDictionaryName :: PredefinedDictionaryName -> Int32
marshalPredefinedDictionaryName = \case
    DICT_4X4_50         -> c'DICT_4X4_50
    DICT_4X4_100        -> c'DICT_4X4_100
    DICT_4X4_250        -> c'DICT_4X4_250
    DICT_4X4_1000       -> c'DICT_4X4_1000
    DICT_5X5_50         -> c'DICT_5X5_50
    DICT_5X5_100        -> c'DICT_5X5_100
    DICT_5X5_250        -> c'DICT_5X5_250
    DICT_5X5_1000       -> c'DICT_5X5_1000
    DICT_6X6_50         -> c'DICT_6X6_50
    DICT_6X6_100        -> c'DICT_6X6_100
    DICT_6X6_250        -> c'DICT_6X6_250
    DICT_6X6_1000       -> c'DICT_6X6_1000
    DICT_7X7_50         -> c'DICT_7X7_50
    DICT_7X7_100        -> c'DICT_7X7_100
    DICT_7X7_250        -> c'DICT_7X7_250
    DICT_7X7_1000       -> c'DICT_7X7_1000
    DICT_ARUCO_ORIGINAL -> c'DICT_ARUCO_ORIGINAL

{-| Turn a predefined dictionary name into a ArUco dictionary.
-}
getPredefinedDictionary :: PredefinedDictionaryName -> Dictionary
getPredefinedDictionary name =
  unsafePerformIO $
  fromPtr $
  [C.block| Ptr_Dictionary * {
    return new Ptr<Dictionary>(getPredefinedDictionary($(int32_t c'name)));
  }|]
  where
    c'name :: Int32
    c'name = marshalPredefinedDictionaryName name

{-| Draw a ChArUco board, ready to be printed and used for calibration/marke
detection.

Example:

@
drawChArUcoBoardImg
    :: forall (w :: Nat) (h :: Nat)
     . (w ~ 500, h ~ 500)
    => Mat ('S '[ 'S h, 'S w]) ('S 1) ('S Word8)
drawChArUcoBoardImg =
    drawChArUcoBoard charucoBoard (Proxy :: Proxy w) (Proxy :: Proxy h)
  where
    charucoBoard :: ChArUcoBoard
    charucoBoard = createChArUcoBoard 10 10 20 5 dictionary

    dictionary :: Dictionary
    dictionary = getPredefinedDictionary DICT_7X7_1000
@

<<doc/generated/examples/drawChArUcoBoardImg.png drawChArUcoBoardImg>>
-}
drawChArUcoBoard
    :: (ToInt32 w, ToInt32 h)
    => ChArUcoBoard
    -> w -- ^ width
    -> h -- ^ height
    -> Mat ('S '[DSNat h, DSNat w]) ('S 1) ('S Word8)
drawChArUcoBoard charucoBoard width height = unsafePerformIO $ do
    dst <- newEmptyMat
    withPtr charucoBoard $ \c'board ->
      withPtr dst $ \dstPtr ->
        [C.block| void {
          Mat & board = * $(Mat * dstPtr);
          Ptr<CharucoBoard> & charucoBoard = *$(Ptr_CharucoBoard * c'board);
          charucoBoard->draw(cv::Size($(int32_t w), $(int32_t h)), board);
        }|]
    pure (unsafeCoerceMat dst)
  where
    w = toInt32 width
    h = toInt32 height

--------------------------------------------------------------------------------

withPtrs
    :: WithPtr a
    => [a] -> (SV.Vector (Ptr (C a)) -> IO b) -> IO b
withPtrs [] io = io mempty
withPtrs (x:xs) io =
    withPtr x $ \ptr -> withPtrs xs $ \sv -> io (SV.singleton ptr <> sv)
