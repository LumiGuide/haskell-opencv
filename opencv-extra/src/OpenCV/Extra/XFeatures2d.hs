{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module OpenCV.Extra.XFeatures2d
    ( -- * SURF
      Surf
    , SurfParams(..)
    , defaultSurfParams
    , mkSurf
    , surfDetectAndCompute

    ) where

import "base" Control.Exception ( mask_ )
import "base" Data.Int
import "base" Data.Word
import "base" Foreign.ForeignPtr ( ForeignPtr, withForeignPtr )
import "base" Foreign.Marshal.Alloc ( alloca )
import "base" Foreign.Marshal.Array ( peekArray )
import "base" Foreign.Marshal.Utils ( fromBool )
import "base" Foreign.Ptr ( Ptr, nullPtr )
import "base" Foreign.Storable ( peek )
import "base" System.IO.Unsafe ( unsafePerformIO )
import qualified "inline-c" Language.C.Inline as C
import qualified "inline-c" Language.C.Inline.Unsafe as CU
import qualified "inline-c-cpp" Language.C.Inline.Cpp as C
import "opencv" OpenCV.Core.Types
import "opencv" OpenCV.Internal
import "opencv" OpenCV.Internal.Core.Types.Mat
import "opencv" OpenCV.Internal.C.Types
import "opencv" OpenCV.Internal.Exception ( cvExcept, unsafeWrapException )
import "opencv" OpenCV.TypeLevel
import "this"   OpenCV.Extra.Internal.C.Inline ( openCvExtraCtx )
import "this"   OpenCV.Extra.Internal.C.Types
import qualified "vector" Data.Vector as V

--------------------------------------------------------------------------------

C.context openCvExtraCtx

C.include "opencv2/core.hpp"
C.include "opencv2/xfeatures2d.hpp"
C.include "xfeatures/surf.hpp"

C.using "namespace cv"

--------------------------------------------------------------------------------
-- SURF - Speeded Up Roubst Features
--------------------------------------------------------------------------------

-- Internally, an Surf is a pointer to a @cv::Ptr<cv::xfeatures2d::SURF>@, which in turn points
-- to an actual @cv::xfeatures2d::SURF@ object.
newtype Surf = Surf {unSurf :: ForeignPtr C'Ptr_SURF}

type instance C Surf = C'Ptr_SURF

instance WithPtr Surf where
    withPtr = withForeignPtr . unSurf

instance FromPtr Surf where
    fromPtr = objFromPtr Surf $ \ptr ->
                [CU.block| void {
                  cv::Ptr<cv::xfeatures2d::SURF> * surf_ptr_ptr = $(Ptr_SURF * ptr);
                  surf_ptr_ptr->release();
                  delete surf_ptr_ptr;
                }|]

--------------------------------------------------------------------------------

data SurfParams
   = SurfParams
     { surf_hessianThreshold :: !Double
       -- ^ Threshold for hessian keypoint detector used in SURF
     , surf_nOctaves :: !Int32
       -- ^ Number of pyramid octaves the keypoint detector will use.
     , surf_nOctaveLayers :: !Int32
       -- ^ Number of octave layers within each octave.
     , surf_extended :: !Bool
       -- ^ Extended descriptor flag (true - use extended 128-element descriptors; false - use 64-element descriptors).
     , surf_upright :: !Bool
       -- ^ Up-right or rotated features flag (true - do not compute orientation of features; false - compute orientation).
     }

defaultSurfParams :: SurfParams
defaultSurfParams =
    SurfParams
     { surf_hessianThreshold = 100
     , surf_nOctaves = 4
     , surf_nOctaveLayers = 3
     , surf_extended = False
     , surf_upright = False
     }

--------------------------------------------------------------------------------

newSurf :: SurfParams -> IO Surf
newSurf SurfParams{..} = fromPtr
    [CU.block|Ptr_SURF * {
      cv::Ptr<cv::xfeatures2d::SURF> surfPtr =
        cv::xfeatures2d::SURF::create
        ( $(double  c'hessianThreshold)
        , $(int32_t surf_nOctaves)
        , $(int32_t surf_nOctaveLayers)
        , $(bool    c'extended)
        , $(bool    c'upright)
        );
      return new cv::Ptr<cv::xfeatures2d::SURF>(surfPtr);
    }|]
  where
    c'hessianThreshold = realToFrac surf_hessianThreshold
    c'extended = fromBool surf_extended
    c'upright = fromBool surf_upright


mkSurf :: SurfParams -> Surf
mkSurf = unsafePerformIO . newSurf

--------------------------------------------------------------------------------

{- | Detect keypoints and compute descriptors

Example:

@
surfDetectAndComputeImg
    :: forall (width    :: Nat)
              (height   :: Nat)
              (channels :: Nat)
              (depth    :: *)
     . (Mat (ShapeT [height, width]) ('S channels) ('S depth) ~ Frog)
    => Mat (ShapeT [height, width]) ('S channels) ('S depth)
surfDetectAndComputeImg = exceptError $ do
    (kpts, _descs) <- surfDetectAndCompute surf frog Nothing
    withMatM (Proxy :: Proxy [height, width])
             (Proxy :: Proxy channels)
             (Proxy :: Proxy depth)
             white $ \\imgM -> do
      void $ matCopyToM imgM (V2 0 0) frog Nothing
      forM_ kpts $ \\kpt -> do
        let kptRec = keyPointAsRec kpt
        circle imgM (round \<$> kptPoint kptRec :: V2 Int32) 5 blue 1 LineType_AA 0
  where
    surf = mkSurf defaultSurfParams
@

<<doc/generated/examples/surfDetectAndComputeImg.png surfDetectAndComputeImg>>
-}
surfDetectAndCompute
    :: Surf
    -> Mat ('S [height, width]) channels depth -- ^ Image.
    -> Maybe (Mat ('S [height, width]) ('S 1) ('S Word8)) -- ^ Mask.
    -> CvExcept ( V.Vector KeyPoint
                , Mat 'D 'D 'D
                )
surfDetectAndCompute surf img mbMask = unsafeWrapException $ do
    descriptors <- newEmptyMat
    withPtr surf $ \surfPtr ->
      withPtr img $ \imgPtr ->
      withPtr mbMask $ \maskPtr ->
      withPtr descriptors $ \descPtr ->
      alloca $ \(numPtsPtr :: Ptr Int32) ->
      alloca $ \(arrayPtrPtr :: Ptr (Ptr (Ptr C'KeyPoint))) -> mask_ $ do
        ptrException <- [cvExcept|
          cv::xfeatures2d::SURF * surf = *$(Ptr_SURF * surfPtr);
          cv::Mat * maskPtr = $(Mat * maskPtr);

          std::vector<cv::KeyPoint> keypoints = std::vector<cv::KeyPoint>();
          surf->
            detectAndCompute
            ( *$(Mat * imgPtr)
            , maskPtr ? cv::_InputArray(*maskPtr) : cv::_InputArray(noArray())
            , keypoints
            , *$(Mat * descPtr)
            , false
            );

          *$(int32_t * numPtsPtr) = keypoints.size();

          cv::KeyPoint * * * arrayPtrPtr = $(KeyPoint * * * arrayPtrPtr);
          cv::KeyPoint * * arrayPtr = new cv::KeyPoint * [keypoints.size()];
          *arrayPtrPtr = arrayPtr;

          for (std::vector<cv::KeyPoint>::size_type ix = 0; ix != keypoints.size(); ix++)
          {
            cv::KeyPoint & org = keypoints[ix];
            cv::KeyPoint * newPt =
              new cv::KeyPoint( org.pt
                              , org.size
                              , org.angle
                              , org.response
                              , org.octave
                              , org.class_id
                              );
            arrayPtr[ix] = newPt;
          }
        |]
        if ptrException /= nullPtr
        then Left . BindingException <$> fromPtr (pure ptrException)
        else do
          numPts <- fromIntegral <$> peek numPtsPtr
          arrayPtr <- peek arrayPtrPtr
          keypoints <- mapM (fromPtr . pure) =<< peekArray numPts arrayPtr

          [CU.block| void {
            delete [] *$(KeyPoint * * * arrayPtrPtr);
          }|]

          pure $ Right (V.fromList keypoints, relaxMat descriptors)


-- vim: ft=haskell
