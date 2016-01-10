{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module OpenCV.Core.Types.Mat.Internal
    ( MatDepth(..)
    , marshalMatDepth
    , marshalFlags
    , unmarshalDepth
    , unmarshalFlags
    , withMatData
    ) where

import "base" Foreign.C.Types
import "base" Foreign.Marshal.Alloc ( alloca )
import "base" Foreign.Marshal.Array ( peekArray )
import "base" Foreign.Ptr ( Ptr )
import "base" Foreign.Storable ( Storable(..), peek )
import qualified "inline-c" Language.C.Inline as C
import qualified "inline-c" Language.C.Inline.Unsafe as CU
import qualified "inline-c-cpp" Language.C.Inline.Cpp as C
import "lumi-hackage-extended" Lumi.Prelude
import "this" Language.C.Inline.OpenCV
import "this" OpenCV.Internal

--------------------------------------------------------------------------------

C.context openCvCtx

C.include "opencv2/core.hpp"
C.using "namespace cv"

#include <bindings.dsl.h>
#include "opencv2/core.hpp"

#include "namespace.hpp"

--------------------------------------------------------------------------------

data MatDepth =
     MatDepth_8U
   | MatDepth_8S
   | MatDepth_16U
   | MatDepth_16S
   | MatDepth_32S
   | MatDepth_32F
   | MatDepth_64F
   | MatDepth_USRTYPE1
     deriving (Show, Eq)

--------------------------------------------------------------------------------

#num CV_CN_MAX
#num CV_CN_SHIFT

#num CV_8U
#num CV_8S
#num CV_16U
#num CV_16S
#num CV_32S
#num CV_32F
#num CV_64F
#num CV_USRTYPE1

#num CV_MAT_DEPTH_MASK

marshalMatDepth :: MatDepth -> Int32
marshalMatDepth = \case
    MatDepth_8U       -> c'CV_8U
    MatDepth_8S       -> c'CV_8S
    MatDepth_16U      -> c'CV_16U
    MatDepth_16S      -> c'CV_16S
    MatDepth_32S      -> c'CV_32S
    MatDepth_32F      -> c'CV_32F
    MatDepth_64F      -> c'CV_64F
    MatDepth_USRTYPE1 -> c'CV_USRTYPE1

marshalFlags
    :: MatDepth
    -> Int32 -- ^ Number of channels
    -> Int32
marshalFlags depth cn =
    marshalMatDepth depth
      .|. ((cn - 1) `unsafeShiftL` c'CV_CN_SHIFT)

unmarshalDepth :: Int32 -> MatDepth
unmarshalDepth n
    | n == c'CV_8U       = MatDepth_8U
    | n == c'CV_8S       = MatDepth_8S
    | n == c'CV_16U      = MatDepth_16U
    | n == c'CV_16S      = MatDepth_16S
    | n == c'CV_32S      = MatDepth_32S
    | n == c'CV_32F      = MatDepth_32F
    | n == c'CV_64F      = MatDepth_64F
    | n == c'CV_USRTYPE1 = MatDepth_USRTYPE1
    | otherwise          = error $ "unknown depth " <> show n

unmarshalFlags :: Int32 -> (MatDepth, Int32)
unmarshalFlags n =
    ( unmarshalDepth $ n .&. c'CV_MAT_DEPTH_MASK
    , 1 + ((n `unsafeShiftR` c'CV_CN_SHIFT) .&. (c'CV_CN_MAX - 1))
    )

--------------------------------------------------------------------------------

withMatData :: Mat -> ([CSize] -> Ptr Word8 -> IO a) -> IO a
withMatData mat f = withMatPtr mat $ \matPtr ->
    alloca $ \(dimsPtr  :: Ptr Int32      ) ->
    alloca $ \(stepPtr2 :: Ptr (Ptr CSize)) ->
    alloca $ \(dataPtr2 :: Ptr (Ptr Word8)) -> do
      [CU.block|void {
        const Mat * const matPtr = $(Mat * matPtr);
        *$(int32_t *   const dimsPtr ) = matPtr->dims;
        *$(size_t  * * const stepPtr2) = matPtr->step.p;
        *$(uint8_t * * const dataPtr2) = matPtr->data;
      }|]
      dims    <- peek dimsPtr
      stepPtr <- peek stepPtr2
      dataPtr <- peek dataPtr2
      step    <- peekArray (fromIntegral dims) stepPtr
      f step dataPtr
