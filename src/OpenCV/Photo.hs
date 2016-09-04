{-# language CPP #-}
{-# language QuasiQuotes #-}
{-# language TemplateHaskell #-}

#if __GLASGOW_HASKELL__ >= 800
{-# options_ghc -Wno-redundant-constraints #-}
#endif

module OpenCV.Photo
  ( InpaintingMethod(..)
  , inpaint
  ) where

import "base" Data.Int ( Int32 )
import "base" Data.Word ( Word8 )
import qualified "inline-c" Language.C.Inline as C
import qualified "inline-c-cpp" Language.C.Inline.Cpp as C
import "this" OpenCV.Internal.C.Inline ( openCvCtx )
import "this" OpenCV.Internal.C.Types ( withPtr )
import "this" OpenCV.Internal.Exception
import "this" OpenCV.Internal.Photo.Constants
import "this" OpenCV.Internal.Core.Types.Mat
import "this" OpenCV.TypeLevel

--------------------------------------------------------------------------------

C.context openCvCtx

C.include "opencv2/core.hpp"
C.include "opencv2/photo.hpp"
C.using "namespace cv"

--------------------------------------------------------------------------------

data InpaintingMethod
   = InpaintNavierStokes
     -- ^ Navier-Stokes based method.
   | InpaintTelea
     -- ^ Method by Alexandru Telea.
     deriving Show

marshalInpaintingMethod :: InpaintingMethod -> Int32
marshalInpaintingMethod = \case
  InpaintNavierStokes -> c'INPAINT_NS
  InpaintTelea        -> c'INPAINT_TELEA

{- | Restores the selected region in an image using the region neighborhood.

Example:

@
inpaintImg
    :: forall h h2 w w2 c d
     . ( Mat (ShapeT [h, w]) ('S c) ('S d) ~ Bikes_512x341
       , h2 ~ ((*) h 2)
       , w2 ~ ((*) w 2)
       )
    => Mat ('S ['S h2, 'S w2]) ('S c) ('S d)
inpaintImg = exceptError $ do
    maskInv <- bitwiseNot mask
    maskBgr <- cvtColor gray bgr maskInv
    damaged <- bitwiseAnd bikes_512x341 maskBgr
    repairedNS <- inpaint 3 InpaintNavierStokes damaged mask
    repairedT  <- inpaint 3 InpaintTelea        damaged mask
    withMatM
      (Proxy :: Proxy [h2, w2])
      (Proxy :: Proxy c)
      (Proxy :: Proxy d)
      black $ \imgM -> do
        matCopyToM imgM (V2 0 0) damaged Nothing
        matCopyToM imgM (V2 w 0) maskBgr Nothing
        matCopyToM imgM (V2 0 h) repairedNS Nothing
        matCopyToM imgM (V2 w h) repairedT  Nothing
  where
    mask = damageMask

    w = fromInteger $ natVal (Proxy :: Proxy w)
    h = fromInteger $ natVal (Proxy :: Proxy h)
@

<<doc/generated/examples/inpaintImg.png inpaintImg>>
-}
inpaint
   :: (channels `In` [1, 3])
   => Double
      -- ^ inpaintRadius - Radius of a circular neighborhood of each
      -- point inpainted that is considered by the algorithm.
   -> InpaintingMethod
   -> Mat ('S [h, w]) ('S channels) ('S Word8) -- ^ Input image.
   -> Mat ('S [h, w]) ('S 1) ('S Word8) -- ^ Inpainting mask.
   -> CvExcept (Mat ('S [h, w]) ('S channels) ('S Word8)) -- ^ Output image.
inpaint inpaintRadius method src inpaintMask = unsafeWrapException $ do
    dst <- newEmptyMat
    handleCvException (pure $ unsafeCoerceMat dst) $
      withPtr src         $ \srcPtr         ->
      withPtr inpaintMask $ \inpaintMaskPtr ->
      withPtr dst         $ \dstPtr         ->
      [cvExcept|
        cv::inpaint( *$(Mat * srcPtr)
                   , *$(Mat * inpaintMaskPtr)
                   , *$(Mat * dstPtr)
                   , $(double c'inpaintRadius)
                   , $(int32_t c'method)
                   );
      |]
  where
    c'method = marshalInpaintingMethod method
    c'inpaintRadius = realToFrac inpaintRadius
