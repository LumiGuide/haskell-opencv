{-# language CPP #-}
{-# language QuasiQuotes #-}
{-# language TemplateHaskell #-}

#if __GLASGOW_HASKELL__ >= 800
{-# options_ghc -Wno-redundant-constraints #-}
#endif

module OpenCV.Extra.XPhoto
  ( dctDenoising
  ) where

import "base" Data.Int ( Int32 )
import "base" Data.Word ( Word8 )
import "base" Data.Maybe ( fromMaybe )
import qualified "inline-c" Language.C.Inline as C
import qualified "inline-c-cpp" Language.C.Inline.Cpp as C
import "opencv" OpenCV.Internal.C.Inline ( openCvCtx )
import "opencv" OpenCV.Internal.C.Types ( withPtr )
import "opencv" OpenCV.Internal.Exception
import "opencv" OpenCV.Internal.Core.Types.Mat
import "opencv" OpenCV.TypeLevel

--------------------------------------------------------------------------------

C.context openCvCtx

C.include "opencv2/core.hpp"
C.include "opencv2/xphoto.hpp"
C.using "namespace cv"

{-| Perform dctDenoising function for colored images.

Example:

@
dctDenoisingImg
    :: forall h w w2 c d
     . ( Mat (ShapeT [h, w]) ('S c) ('S d) ~ Lenna_512x512
       , w2 ~ ((*) w 2)
       )
    => Mat ('S ['S h, 'S w2]) ('S c) ('S d)
dctDenoisingImg = exceptError $ do
    denoised <- dctDenoising 10 Nothing lenna_512x512
    withMatM
      (Proxy :: Proxy [h, w2])
      (Proxy :: Proxy c)
      (Proxy :: Proxy d)
      black $ \\imgM -> do
        matCopyToM imgM (V2 0 0) lenna_512x512 Nothing
        matCopyToM imgM (V2 w 0) denoised Nothing
  where
    w = fromInteger $ natVal (Proxy :: Proxy w)
@

<<doc/generated/examples/dctDenoisingImg.png dctDenoisingImg>>
-}

dctDenoising
   :: Double -- ^ expected noise standard deviation
   -> Maybe Int32  -- ^ size of block side where dct is computed use default 16
   -> Mat ('S [h, w]) ('S 3) ('S Word8) -- ^ Input image 8-bit 3-channel image.
   -> CvExcept (Mat ('S [h, w]) ('S 3) ('S Word8))
             -- ^ Output image same size and type as input.
dctDenoising sigma mPSize src =
  unsafeWrapException $ do
    dst <- newEmptyMat
    handleCvException (pure $ unsafeCoerceMat dst) $
      withPtr src         $ \srcPtr         ->
      withPtr dst         $ \dstPtr         ->
      [cvExcept|
        cv::xphoto::dctDenoising( *$(Mat * srcPtr)
                                , *$(Mat * dstPtr)
                                , $(double c'sigma)
                                , $(int32_t c'pSize)
                                );
      |]
  where
    c'sigma = realToFrac sigma
    c'pSize = fromMaybe 16 mPSize
