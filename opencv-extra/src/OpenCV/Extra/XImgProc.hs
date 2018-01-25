{-# language TemplateHaskell #-}
{-# language QuasiQuotes #-}

module OpenCV.Extra.XImgProc
    ( anisotropicDiffusion
    ) where

import "base" Data.Int
import "base" Data.Word
import qualified "inline-c" Language.C.Inline as C
import qualified "inline-c-cpp" Language.C.Inline.Cpp as C
import "opencv" OpenCV.Core.Types
import "opencv" OpenCV.Internal.Core.Types.Mat
import "opencv" OpenCV.Internal.C.Types
import "opencv" OpenCV.Internal.Exception
import "opencv" OpenCV.TypeLevel
import "this"   OpenCV.Extra.Internal.C.Inline ( openCvExtraCtx )

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
