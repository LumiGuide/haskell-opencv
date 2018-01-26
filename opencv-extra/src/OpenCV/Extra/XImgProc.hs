{-# language TemplateHaskell #-}
{-# language QuasiQuotes #-}

module OpenCV.Extra.XImgProc
    ( anisotropicDiffusion
    , BinarizationMethod(..)
    , niBlackThreshold
    ) where

import "base" Data.Int
import "base" Data.Word
import qualified "inline-c" Language.C.Inline as C
import qualified "inline-c-cpp" Language.C.Inline.Cpp as C
import "opencv" OpenCV.Core.Types
import "opencv" OpenCV.Internal.Core.Types.Mat
import "opencv" OpenCV.Internal.C.Types
import "opencv" OpenCV.Internal.Exception
import "opencv" OpenCV.Internal.ImgProc.MiscImgTransform
import "opencv" OpenCV.TypeLevel
import "this"   OpenCV.Extra.Internal.C.Inline ( openCvExtraCtx )
import "this"   OpenCV.Extra.Internal.XImgProc.Constants

--------------------------------------------------------------------------------

C.context openCvExtraCtx

C.include "opencv2/core.hpp"
C.include "opencv2/ximgproc.hpp"

C.using "namespace cv"

--------------------------------------------------------------------------------

{- | Performs anisotropic diffusion on an image.

The function applies Perona-Malik anisotropic diffusion to an
image. This is the solution to the partial differential equation:

\[
{\frac{\partial I}{\partial t}}={\mathrm{div}}\left(c(x,y,t)\nabla I\right)=\nabla c\cdot \nabla I+c(x,y,t)\Delta I
\]

Suggested functions for \(c(x,y,t)\) are:

\[
c\left(\|\nabla I\|\right)=e^{ { -\left(\|\nabla I\|/K\right)^{2}} }
\]

or

\[
c\left(\|\nabla I\|\right)={\frac {1}{1+\left({\frac {\|\nabla I\|}{K}}\right)^{2}}}
\]

Example:

@
anisotropicDiffusionImg :: Kodak_768x512
anisotropicDiffusionImg = exceptError $ anisotropicDiffusion 1.0 0.02 10 birds_768x512
@

<<doc/generated/examples/anisotropicDiffusionImg.png anisotropicDiffusionImg>>
-}
anisotropicDiffusion
    :: Float
       -- ^ The amount of time to step forward by on each iteration (normally,
       -- it's between 0 and 1). Must be > 0.
    -> Float -- ^ Sensitivity to the edges. Must be /= 0.
    -> Int32 -- ^ The number of iterations. Must be >= 0.
    -> Mat ('S '[h, w]) ('S 3) ('S Word8)
    -> CvExcept (Mat ('S '[h, w]) ('S 3) ('S Word8))
anisotropicDiffusion alpha k niters src = unsafeWrapException $ do
    dst <- newEmptyMat
    handleCvException (pure $ unsafeCoerceMat dst) $
      withPtr src $ \srcPtr ->
      withPtr dst $ \dstPtr ->
        [cvExcept|
          cv::ximgproc::anisotropicDiffusion
            ( *$(Mat * srcPtr)
            , *$(Mat * dstPtr)
            , $(float c'alpha)
            , $(float c'k)
            , $(int32_t niters)
            );
        |]
  where
    c'alpha = toCFloat alpha
    c'k     = toCFloat k

data BinarizationMethod
   = Binarization_NIBLACK -- ^ Classic Niblack binarization.
   | Binarization_SAUVOLA -- ^ Sauvola's technique.
   | Binarization_WOLF    -- ^ Wolf's technique.
   | Binarization_NICK    -- ^ NICK technique.
     deriving (Show)

marshalBinarizationMethod :: BinarizationMethod -> Int32
marshalBinarizationMethod = \case
    Binarization_NIBLACK  -> c'BINARIZATION_NIBLACK
    Binarization_SAUVOLA  -> c'BINARIZATION_SAUVOLA
    Binarization_WOLF     -> c'BINARIZATION_WOLF
    Binarization_NICK     -> c'BINARIZATION_NICK

{- | Performs thresholding on input images using Niblack's technique or
some of the popular variations it inspired.

Example:

@
niBlackThresholdExample
    :: forall shape channels depth. (Mat shape channels depth ~ CodexLeicester)
    => BinarizationMethod -> Mat shape ('S 1) depth
niBlackThresholdExample binarizationMethod = exceptError $ do
    codexLeicesterG <- cvtColor bgr gray codexLeicester
    niBlackThreshold (Thresh_Binary 255) 51 0.1 binarizationMethod codexLeicesterG

codexLeicesterNIBLACK
    :: forall shape channels depth. (Mat shape channels depth ~ CodexLeicester)
    => Mat shape ('S 1) depth
codexLeicesterNIBLACK = niBlackThresholdExample Binarization_NIBLACK

codexLeicesterSAUVOLA
    :: forall shape channels depth. (Mat shape channels depth ~ CodexLeicester)
    => Mat shape ('S 1) depth
codexLeicesterSAUVOLA = niBlackThresholdExample Binarization_SAUVOLA
@

<<data/codex_leicester.jpg Excerpt from the Codex Leicester by Leonardo da Vinci>>
<<doc/generated/examples/codexLeicesterNIBLACK.png codexLeicesterNIBLACK>>
<<doc/generated/examples/codexLeicesterSAUVOLA.png codexLeicesterSAUVOLA>>
-}
niBlackThreshold
    :: ThreshType
    -> Int32
       -- ^ Size of a pixel neighborhood that is used to calculate a
       -- threshold value for the pixel. Must be odd and >= 1.
    -> Double
       -- ^ The user-adjustable parameter used by
       -- 'Binarization_NIBLACK' and inspired techniques. For
       -- 'Binarization_NIBLACK', this is normally a value between 0
       -- and 1 that is multiplied with the standard deviation and
       -- subtracted from the mean.
    -> BinarizationMethod -- ^ Binarization method to use.
    -> Mat ('S '[h, w]) ('S 1) ('S Word8) -- ^ Source image.
    -> CvExcept (Mat ('S '[h, w]) ('S 1) ('S Word8))
niBlackThreshold threshType blockSize k binarizationMethod src = unsafeWrapException $ do
    dst <- newEmptyMat
    handleCvException (pure $ unsafeCoerceMat dst) $
      withPtr src $ \srcPtr ->
      withPtr dst $ \dstPtr ->
        [cvExcept|
          cv::ximgproc::niBlackThreshold
            ( *$(Mat * srcPtr)
            , *$(Mat * dstPtr)
            , $(double maxValue)
            , $(int32_t c'threshType)
            , $(int32_t blockSize)
            , $(double c'k)
            , $(int32_t c'binarizationMethod)
            );
        |]
  where
    (c'threshType, maxValue) = marshalThreshType threshType
    c'binarizationMethod = marshalBinarizationMethod binarizationMethod
    c'k = toCDouble k
