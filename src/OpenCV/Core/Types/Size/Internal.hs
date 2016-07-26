{-# language CPP             #-}
{-# language ConstraintKinds #-}
{-# language QuasiQuotes     #-}
{-# language TemplateHaskell #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module OpenCV.Core.Types.Size.Internal
  ( Size(..)
  , SizeDepth
  , SizeDim
  , ToSize(..), FromSize(..)

  , Size2i, Size2f, Size2d

  , ToSize2i  , ToSize2f  , ToSize2d
  , FromSize2i, FromSize2f, FromSize2d

  , newSize2i, newSize2f, newSize2d
  ) where

import "base" Data.Int ( Int32 )
import "base" Foreign.C.Types
import "base" Foreign.ForeignPtr ( ForeignPtr, withForeignPtr )
import "base" Foreign.Marshal.Alloc ( alloca )
import "base" Foreign.Storable ( peek )
import "base" GHC.TypeLits
import "base" System.IO.Unsafe ( unsafePerformIO )
import qualified "inline-c" Language.C.Inline as C
import qualified "inline-c" Language.C.Inline.Unsafe as CU
import qualified "inline-c-cpp" Language.C.Inline.Cpp as C
import "linear" Linear.V2 ( V2(..) )
import "this" OpenCV.C.Inline ( openCvCtx )
import "this" OpenCV.C.PlacementNew.TH
import "this" OpenCV.C.Types
import "this" OpenCV.Internal

--------------------------------------------------------------------------------

C.context openCvCtx

C.include "opencv2/core.hpp"
C.using "namespace cv"

--------------------------------------------------------------------------------

newtype Size (depth :: *) (dim :: Nat)
      = Size {unSize :: ForeignPtr (C'Size depth dim)}

type instance C (Size depth dim) = C'Size depth dim

instance WithPtr (Size depth dim) where withPtr = withForeignPtr . unSize

type family SizeDepth (a :: *) :: *

type family SizeDim (a :: *) :: Nat

-- TODO (RvD): rename to TNormDepth and move to Depth module
type family TNormSizeDepth (a :: *) :: * where
    TNormSizeDepth CFloat = CFloat
    TNormSizeDepth Float  = CFloat

    TNormSizeDepth CDouble = CDouble
    TNormSizeDepth Double  = CDouble

    TNormSizeDepth Int32 = Int32

class ToSize   a where toSize   :: a -> Size (SizeDepth a) (SizeDim a)
class FromSize a where fromSize :: Size (SizeDepth a) (SizeDim a) -> a

#define SIZE_TYPE(NAME, HDEPTH, DIM)                      \
type NAME = Size HDEPTH DIM;                              \
instance FromPtr NAME where {                             \
    fromPtr = objFromPtr Size $ \ptr ->                   \
                [CU.exp| void { delete $(NAME * ptr) }|]; \
};

#define SIZE2_TYPE(NAME, TO_NAME, FROM_NAME, NEW_NAME, HDEPTH, CDEPTH) \
SIZE_TYPE(NAME, HDEPTH, 2)                                             \
type TO_NAME   p = (ToSize   p, SizeDim p ~ 2, SizeDepth p ~ HDEPTH);  \
type FROM_NAME p = (FromSize p, SizeDim p ~ 2, SizeDepth p ~ HDEPTH);  \
instance FromSize (V2 HDEPTH) where {                                  \
    fromSize size = unsafePerformIO $                                  \
      alloca $ \xPtr ->                                                \
      alloca $ \yPtr ->                                                \
      withPtr size $ \sizePtr -> do {                                  \
        [CU.block| void {                                              \
          const cv::Size_<CDEPTH> & p = *$(NAME * sizePtr);            \
          *$(CDEPTH * xPtr) = p.width;                                 \
          *$(CDEPTH * yPtr) = p.height;                                \
        }|];                                                           \
        V2 <$> peek xPtr                                               \
           <*> peek yPtr;                                              \
      };                                                               \
};                                                                     \
NEW_NAME :: V2 HDEPTH -> IO NAME;                                      \
NEW_NAME (V2 x y) = fromPtr $                                          \
    [CU.exp|NAME * { new cv::Size_<CDEPTH>($(CDEPTH x), $(CDEPTH y)) }|];

SIZE2_TYPE(Size2i, ToSize2i, FromSize2i, newSize2i, Int32  , int32_t)
SIZE2_TYPE(Size2f, ToSize2f, FromSize2f, newSize2f, CFloat , float  )
SIZE2_TYPE(Size2d, ToSize2d, FromSize2d, newSize2d, CDouble, double )

type instance SizeDepth (Size depth dim) = depth
type instance SizeDim   (Size depth dim) = dim

type instance SizeDepth (V2 a) = TNormSizeDepth a
type instance SizeDim   (V2 a) = 2

instance ToSize (Size depth dim) where toSize = id

instance ToSize (V2 Int32  ) where toSize = unsafePerformIO . newSize2i
instance ToSize (V2 CFloat ) where toSize = unsafePerformIO . newSize2f
instance ToSize (V2 Float  ) where toSize = toSize . fmap (realToFrac :: Float  -> CFloat )
instance ToSize (V2 CDouble) where toSize = unsafePerformIO . newSize2d
instance ToSize (V2 Double ) where toSize = toSize . fmap (realToFrac :: Double  -> CDouble )

instance FromSize (Size depth dim) where fromSize = id

instance FromSize (V2 Float ) where fromSize = fmap (realToFrac :: CFloat  -> Float ) . fromSize
instance FromSize (V2 Double) where fromSize = fmap (realToFrac :: CDouble -> Double) . fromSize

--------------------------------------------------------------------------------

instance Show Size2i where
    showsPrec prec size = showParen (prec >= 10) $
                               showString "fromSize2i "
                             . shows (fromSize size :: V2 Int32)

--------------------------------------------------------------------------------

mkPlacementNewInstance ''Size2i
mkPlacementNewInstance ''Size2f
mkPlacementNewInstance ''Size2d
