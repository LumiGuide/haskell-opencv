{-# language QuasiQuotes #-}
{-# language TemplateHaskell #-}

module OpenCV.ImgProc.MotionAnalysis
    ( accumulate
    , accumulateProduct
    , accumulateSquare
    , accumulateWeighted
    ) where

import "base" Data.Word
import qualified "inline-c-cpp" Language.C.Inline.Cpp as C
import "mtl" Control.Monad.Error.Class ( MonadError )
import "primitive" Control.Monad.Primitive ( PrimMonad, PrimState, unsafePrimToPrim )
import "this" OpenCV.Core.Types
import "this" OpenCV.Internal.C.Inline ( openCvCtx )
import "this" OpenCV.Internal.C.Types
import "this" OpenCV.Internal.Exception
import "this" OpenCV.TypeLevel

 --------------------------------------------------------------------------------

C.context openCvCtx

C.include "opencv2/core.hpp"
C.include "opencv2/imgproc.hpp"
C.using "namespace cv"

--------------------------------------------------------------------------------

{- | Adds an image to the accumulator image.

The function adds @src@ or some of its elements to @dst@:

\[
\texttt{dst} (x,y) \leftarrow \texttt{dst} (x,y) + \texttt{src} (x,y)
\quad \text{if} \quad \texttt{mask} (x,y) \ne 0
\]

The function supports multi-channel images. Each channel is processed independently.

The function 'accumulate' can be used, for example, to collect statistics of a
scene background viewed by a still camera and for the further
foreground-background segmentation.
-}
accumulate
    :: ( srcDepth `In` '[ Word8, Word16, Float, Double ]
       , dstDepth `In` '[ Float, Double ]
       , PrimMonad m
       , MonadError CvException m
       )
    => Mat ('S '[ height, width ]) channels ('S srcDepth)
       -- ^ @src@: Input image.
    -> Mut (Mat ('S '[ height, width ]) channels ('S dstDepth)) (PrimState m)
       -- ^ @dst@: Accumulator image.
    -> Maybe (Mat ('S '[ height, width ]) ('S 1) ('S Word8))
       -- ^ @mask@: Optional operation mask.
    -> m ()
accumulate src dst mbMask = wrapException $ unsafePrimToPrim $
    withPtr src    $ \srcPtr ->
    withPtr dst    $ \dstPtr ->
    withPtr mbMask $ \maskPtr ->
      handleCvException (pure ()) $
        [cvExcept|
          Mat * maskPtr = $(Mat * maskPtr);
          cv::accumulate
          ( *$(Mat * srcPtr)
          , *$(Mat * dstPtr)
          , maskPtr ? _InputArray(*maskPtr) : _InputArray(cv::noArray())
          );
        |]

{- | Adds the per-element product of two input images to the accumulator image.

The function adds the product of two images or their selected regions to the
accumulator @dst@:

\[
\texttt{dst} (x,y) \leftarrow \texttt{dst} (x,y) +
\texttt{src1} (x,y) \cdot \texttt{src2} (x,y)
\quad \text{if} \quad \texttt{mask} (x,y)  \ne 0
\]

The function supports multi-channel images. Each channel is processed
independently.
-}
accumulateProduct
    :: ( srcDepth `In` '[ Word8, Float ]
       , dstDepth `In` '[ Float, Double ]
       , channels `In` '[ 1, 3 ]
       , PrimMonad m
       , MonadError CvException m
       )
    => Mat ('S '[ height, width ]) ('S channels) ('S srcDepth)
       -- ^ @src1@: First input image.
    -> Mat ('S '[ height, width ]) ('S channels) ('S srcDepth)
       -- ^ @src2@: Second input image.
    -> Mut (Mat ('S '[ height, width ]) ('S channels) ('S dstDepth)) (PrimState m)
       -- ^ @dst@: Accumulator image.
    -> Maybe (Mat ('S '[ height, width ]) ('S 1) ('S Word8))
       -- ^ @mask@: Optional operation mask.
    -> m ()
accumulateProduct src1 src2 dst mbMask = wrapException $ unsafePrimToPrim $
    withPtr src1   $ \src1Ptr ->
    withPtr src2   $ \src2Ptr ->
    withPtr dst    $ \dstPtr ->
    withPtr mbMask $ \maskPtr ->
      handleCvException (pure ()) $
        [cvExcept|
          Mat * maskPtr = $(Mat * maskPtr);
          cv::accumulateProduct
          ( *$(Mat * src1Ptr)
          , *$(Mat * src2Ptr)
          , *$(Mat * dstPtr)
          , maskPtr ? _InputArray(*maskPtr) : _InputArray(cv::noArray())
          );
        |]

{- | Adds the square of a source image to the accumulator image.

The function adds the input image @src@ or its selected region, raised to a
power of 2, to the accumulator @dst@:

\[
\texttt{dst} (x,y) \leftarrow \texttt{dst} (x,y) + \texttt{src} (x,y)^2
\quad \text{if} \quad \texttt{mask} (x,y) \ne 0
\]

The function supports multi-channel images. Each channel is processed
independently.
-}
accumulateSquare
    :: ( srcDepth `In` '[ Word8, Double ]
       , dstDepth `In` '[ Float, Double ]
       , channels `In` '[ 1, 3 ]
       , PrimMonad m
       , MonadError CvException m
       )
    => Mat ('S '[ height, width ]) ('S channels) ('S srcDepth)
       -- ^ @src@: Input image.
    -> Mut (Mat ('S '[ height, width ]) ('S channels) ('S dstDepth)) (PrimState m)
       -- ^ @dst@: Accumulator image.
    -> Maybe (Mat ('S '[ height, width ]) ('S 1) ('S Word8))
       -- ^ @mask@: Optional operation mask.
    -> m ()
accumulateSquare src dst mbMask = wrapException $ unsafePrimToPrim $
    withPtr src    $ \srcPtr ->
    withPtr dst    $ \dstPtr ->
    withPtr mbMask $ \maskPtr ->
      handleCvException (pure ()) $
        [cvExcept|
          Mat * maskPtr = $(Mat * maskPtr);
          cv::accumulateSquare
          ( *$(Mat * srcPtr)
          , *$(Mat * dstPtr)
          , maskPtr ? _InputArray(*maskPtr) : _InputArray(cv::noArray())
          );
        |]

{- | Updates a running average.

The function calculates the weighted sum of the input image src and the
accumulator dst so that @dst@ becomes a running average of a frame sequence:

\[
\texttt{dst} (x,y) \leftarrow (1- \texttt{alpha} )
\cdot \texttt{dst} (x,y) + \texttt{alpha}
\cdot \texttt{src} (x,y)
\quad \text{if} \quad \texttt{mask} (x,y) \ne 0
\]

That is, @alpha@ regulates the update speed (how fast the accumulator "forgets"
about earlier images). The function supports multi-channel images. Each channel
is processed independently.
-}
accumulateWeighted
    :: ( srcDepth `In` '[ Word8, Double ]
       , dstDepth `In` '[ Float, Double ]
       , channels `In` '[ 1, 3 ]
       , PrimMonad m
       , MonadError CvException m
       )
    => Mat ('S '[ height, width ]) ('S channels) ('S srcDepth)
       -- ^ @src@: Input image.
    -> Mut (Mat ('S '[ height, width ]) ('S channels) ('S dstDepth)) (PrimState m)
       -- ^ @dst@: Accumulator image.
    -> Double -- ^ @alpha@: Weight of the input image.
    -> Maybe (Mat ('S '[ height, width ]) ('S 1) ('S Word8))
       -- ^ @mask@: Optional operation mask.
    -> m ()
accumulateWeighted src dst alpha mbMask = wrapException $ unsafePrimToPrim $
    withPtr src    $ \srcPtr ->
    withPtr dst    $ \dstPtr ->
    withPtr mbMask $ \maskPtr ->
      handleCvException (pure ()) $
        [cvExcept|
          Mat * maskPtr = $(Mat * maskPtr);
          cv::accumulateWeighted
          ( *$(Mat * srcPtr)
          , *$(Mat * dstPtr)
          , $(double c'alpha)
          , maskPtr ? _InputArray(*maskPtr) : _InputArray(cv::noArray())
          );
        |]
  where
    c'alpha = toCDouble alpha
