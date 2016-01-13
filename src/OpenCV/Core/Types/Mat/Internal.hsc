{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module OpenCV.Core.Types.Mat.Internal
    ( Mat(..)
    , MutMat(..)
    , MatDepth(..)
    , matFromPtr
    , withMatPtr
    , withMbMatPtr
    , keepMatAliveDuring
    , newEmptyMat
    , newMat
    , marshalMatDepth
    , marshalFlags
    , unmarshalDepth
    , unmarshalFlags
    , withMatData
    , matElemAddress
    ) where

import "base" Foreign.C.Types
import "base" Foreign.ForeignPtr ( ForeignPtr, withForeignPtr, touchForeignPtr )
import "base" Foreign.Marshal.Alloc ( alloca )
import "base" Foreign.Marshal.Array ( allocaArray, peekArray )
import "base" Foreign.Ptr ( Ptr, nullPtr, plusPtr )
import "base" Foreign.Storable ( Storable(..), peek )
import qualified "inline-c" Language.C.Inline as C
import qualified "inline-c" Language.C.Inline.Unsafe as CU
import qualified "inline-c-cpp" Language.C.Inline.Cpp as C
import "lumi-hackage-extended" Lumi.Prelude
import "this" Language.C.Inline.OpenCV
import "this" OpenCV.Core.Types.Internal
import "this" OpenCV.Internal
import qualified "vector" Data.Vector as V
import qualified "vector" Data.Vector.Generic as VG

--------------------------------------------------------------------------------

C.context openCvCtx

C.include "opencv2/core.hpp"
C.using "namespace cv"

#include <bindings.dsl.h>
#include "opencv2/core.hpp"

#include "namespace.hpp"

--------------------------------------------------------------------------------

newtype Mat = Mat {unMat :: ForeignPtr C'Mat}
newtype MutMat s = MutMat { unMutMat :: Mat }


matFromPtr :: IO (Ptr C'Mat) -> IO Mat
matFromPtr = objFromPtr Mat $ \ptr -> [CU.exp| void { delete $(Mat * ptr) }|]

withMatPtr :: Mat -> (Ptr C'Mat -> IO a) -> IO a
withMatPtr = withForeignPtr . unMat

withMbMatPtr :: Maybe Mat -> (Ptr C'Mat -> IO a) -> IO a
withMbMatPtr mbMat f =
    case mbMat of
      Just mat -> withMatPtr mat f
      Nothing  -> f nullPtr

-- | Similar to 'withMatPtr' in that it keeps the 'ForeignPtr' alive
-- during the execution of the given action but it doesn't extract the 'Ptr'
-- from the 'ForeignPtr'.
keepMatAliveDuring :: Mat -> IO a -> IO a
keepMatAliveDuring mat m = do
    x <- m
    touchForeignPtr $ unMat mat
    pure x

newEmptyMat :: IO Mat
newEmptyMat = matFromPtr [CU.exp|Mat * { new Mat() }|]

newMat
    :: (ToScalar scalar)
    => V.Vector Int32 -- ^ Vector of sizes
    -> MatDepth
    -> Int32        -- ^ Number of channels
    -> scalar       -- ^ Default element value
    -> IO Mat
newMat sizes matDepth cn defValue =
    withVector sizes $ \sizesPtr ->
    withScalarPtr defValue $ \scalarPtr ->
      matFromPtr [CU.exp|Mat * {
        new Mat( $(int32_t c'ndims)
               , $(int32_t * sizesPtr)
               , $(int32_t c'type)
               , *$(Scalar * scalarPtr)
               )
      }|]
  where
    c'ndims = fromIntegral $ VG.length sizes
    c'type  = marshalFlags matDepth cn

-- TODO (BvD): Move to some Utility module.
withVector
    :: (VG.Vector v a, Storable a)
    => v a
    -> (Ptr a -> IO b)
    -> IO b
withVector v f =
    allocaArray n $ \ptr ->
      let go !ix
              | ix < n = do
                  pokeElemOff ptr ix (VG.unsafeIndex v ix)
                  go (ix+1)
              | otherwise = f ptr
      in go 0
  where
    n = VG.length v


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

matElemAddress :: Ptr Word8 -> [Int] -> [Int] -> Ptr a
matElemAddress dataPtr step pos = dataPtr `plusPtr` offset
    where
      offset = sum $ zipWith (*) step pos
