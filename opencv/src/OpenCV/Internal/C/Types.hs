{-# language CPP #-}

#ifndef ENABLE_INTERNAL_DOCUMENTATION
{-# OPTIONS_HADDOCK hide #-}
#endif

module OpenCV.Internal.C.Types where

import "base" Foreign.C.Types
import "base" Foreign.Ptr ( Ptr, nullPtr )
import "base" Data.Int ( Int32 )
import "base" GHC.TypeLits
import "this" OpenCV.Internal.Core.Types.Constants
import "this" OpenCV.Internal.Mutable

--------------------------------------------------------------------------------

data C'Matx  (dimR :: Nat) (dimC :: Nat) (depth :: *)
data C'Vec   (dim :: Nat) (depth :: *)
data C'Point (dim :: Nat) (depth :: *)
data C'Size  (depth :: *)
data C'Rect  (depth :: *)

type C'Matx12f = C'Matx 1 2 CFloat
type C'Matx12d = C'Matx 1 2 CDouble
type C'Matx13f = C'Matx 1 3 CFloat
type C'Matx13d = C'Matx 1 3 CDouble
type C'Matx14f = C'Matx 1 4 CFloat
type C'Matx14d = C'Matx 1 4 CDouble
type C'Matx16f = C'Matx 1 6 CFloat
type C'Matx16d = C'Matx 1 6 CDouble
type C'Matx21f = C'Matx 2 1 CFloat
type C'Matx21d = C'Matx 2 1 CDouble
type C'Matx22f = C'Matx 2 2 CFloat
type C'Matx22d = C'Matx 2 2 CDouble
type C'Matx23f = C'Matx 2 3 CFloat
type C'Matx23d = C'Matx 2 3 CDouble
type C'Matx31f = C'Matx 3 1 CFloat
type C'Matx31d = C'Matx 3 1 CDouble
type C'Matx32f = C'Matx 3 2 CFloat
type C'Matx32d = C'Matx 3 2 CDouble
type C'Matx33f = C'Matx 3 3 CFloat
type C'Matx33d = C'Matx 3 3 CDouble
type C'Matx34f = C'Matx 3 4 CFloat
type C'Matx34d = C'Matx 3 4 CDouble
type C'Matx41f = C'Matx 4 1 CFloat
type C'Matx41d = C'Matx 4 1 CDouble
type C'Matx43f = C'Matx 4 3 CFloat
type C'Matx43d = C'Matx 4 3 CDouble
type C'Matx44f = C'Matx 4 4 CFloat
type C'Matx44d = C'Matx 4 4 CDouble
type C'Matx51f = C'Matx 5 1 CFloat
type C'Matx51d = C'Matx 5 1 CDouble
type C'Matx61f = C'Matx 6 1 CFloat
type C'Matx61d = C'Matx 6 1 CDouble
type C'Matx66f = C'Matx 6 6 CFloat
type C'Matx66d = C'Matx 6 6 CDouble

type C'Vec2i = C'Vec 2 Int32
type C'Vec2f = C'Vec 2 CFloat
type C'Vec2d = C'Vec 2 CDouble

type C'Vec3i = C'Vec 3 Int32
type C'Vec3f = C'Vec 3 CFloat
type C'Vec3d = C'Vec 3 CDouble

type C'Vec4i = C'Vec 4 Int32
type C'Vec4f = C'Vec 4 CFloat
type C'Vec4d = C'Vec 4 CDouble

type C'Point2i = C'Point 2 Int32
type C'Point2f = C'Point 2 CFloat
type C'Point2d = C'Point 2 CDouble

type C'Point3i = C'Point 3 Int32
type C'Point3f = C'Point 3 CFloat
type C'Point3d = C'Point 3 CDouble

type C'Size2i = C'Size Int32
type C'Size2f = C'Size CFloat
type C'Size2d = C'Size CDouble

type C'Rect2i = C'Rect Int32
type C'Rect2f = C'Rect CFloat
type C'Rect2d = C'Rect CDouble


-- | Haskell representation of an OpenCV exception
data C'CvCppException
-- | Haskell representation of an OpenCV @cv::String@ object
data C'CvString
-- | Haskell representation of an OpenCV @cv::RotatedRect@ object
data C'RotatedRect
-- | Haskell representation of an OpenCV @cv::TermCriteria@ object
data C'TermCriteria
-- | Haskell representation of an OpenCV @cv::Range@ object
data C'Range
-- | Haskell representation of an OpenCV @cv::Scalar_\<double>@ object
data C'Scalar
-- | Haskell representation of an OpenCV @cv::Mat@ object
data C'Mat

-- | Haskell representation of an OpenCV @cv::Keypoint@ object
data C'KeyPoint
-- | Haskell representation of an OpenCV @cv::DMatch@ object
data C'DMatch

-- -- | Haskell representation of an OpenCV @cv::MSER@ object
-- data C'MSER
-- | Haskell representation of an OpenCV @cv::Ptr<cv::ORB>@ object
data C'Ptr_ORB
-- -- | Haskell representation of an OpenCV @cv::BRISK@ object
-- data C'BRISK
-- -- | Haskell representation of an OpenCV @cv::KAZE@ object
-- data C'KAZE
-- -- | Haskell representation of an OpenCV @cv::AKAZE@ object
-- data C'AKAZE
-- | Haskell representation of an OpenCV @cv::Ptr<cv::SimpleBlobDetector>@ object
data C'Ptr_SimpleBlobDetector

-- | Haskell representation of an OpenCV @cv::DescriptorMatcher@ object
data C'DescriptorMatcher
-- | Haskell representation of an OpenCV @cv::BFMatcher@ object
data C'BFMatcher
-- | Haskell representation of an OpenCV @cv::FlannBasedMatcher@ object
data C'FlannBasedMatcher

-- | Haskell representation of an OpenCV @cv::Ptr<cv::BackgroundSubtractorMOG2>@ object
data C'Ptr_BackgroundSubtractorKNN
-- | Haskell representation of an OpenCV @cv::Ptr<cv::BackgroundSubtractorKNN>@ object
data C'Ptr_BackgroundSubtractorMOG2

-- | Haskell representation of an OpenCV @cv::VideoCapture@ object
data C'VideoCapture

-- | Haskell representation of an OpenCV @cv::VideoWriter@ object
data C'VideoWriter

-- | Haskell representation of an OpenCV @cv::CascadeClassifier@ object
data C'CascadeClassifier

-- | Callback function for mouse events
type C'MouseCallback
   =  Int32 -- ^ One of the @cv::MouseEvenTypes@ constants.
   -> Int32 -- ^ The x-coordinate of the mouse event.
   -> Int32 -- ^ The y-coordinate of the mouse event.
   -> Int32 -- ^ One of the @cv::MouseEventFlags@ constants.
   -> Ptr () -- ^ Optional pointer to user data.
   -> IO ()

-- | Callback function for trackbars
type C'TrackbarCallback
   =  Int32 -- ^ Current position of the specified trackbar.
   -> Ptr () -- ^ Optional pointer to user data.
   -> IO ()

--------------------------------------------------------------------------------

-- | Haskell representation of an OpenCV @cv::dnn::Net@ object
data C'Net

-- | Haskell representation of an OpenCV @cv::dnn::DictValue@ object
data C'DictValue

--------------------------------------------------------------------------------

-- | Information about the storage requirements of values in C
--
-- This class assumes that the type @a@ is merely a symbol that corresponds with
-- a type in C.
class CSizeOf a where
    -- | Computes the storage requirements (in bytes) of values of
    -- type @a@ in C.
    cSizeOf :: proxy a -> Int

instance CSizeOf C'Point2i where cSizeOf _proxy = c'sizeof_Point2i
instance CSizeOf C'Point2f where cSizeOf _proxy = c'sizeof_Point2f
instance CSizeOf C'Point2d where cSizeOf _proxy = c'sizeof_Point2d
instance CSizeOf C'Point3i where cSizeOf _proxy = c'sizeof_Point3i
instance CSizeOf C'Point3f where cSizeOf _proxy = c'sizeof_Point3f
instance CSizeOf C'Point3d where cSizeOf _proxy = c'sizeof_Point3d
instance CSizeOf C'Size2i  where cSizeOf _proxy = c'sizeof_Size2i
instance CSizeOf C'Size2f  where cSizeOf _proxy = c'sizeof_Size2f
instance CSizeOf C'Scalar  where cSizeOf _proxy = c'sizeof_Scalar
instance CSizeOf C'Range   where cSizeOf _proxy = c'sizeof_Range
instance CSizeOf C'Mat     where cSizeOf _proxy = c'sizeof_Mat

-- | Equivalent type in C
--
-- Actually a proxy type in Haskell that stands for the equivalent type in C.
type family C (a :: *) :: *

type instance C (Maybe a) = C a

-- | Mutable types have the same C equivalent as their unmutable variants.
type instance C (Mut a s) = C a

--------------------------------------------------------------------------------

-- | Perform an IO action with a pointer to the C equivalent of a value
class WithPtr a where
    -- | Perform an action with a temporary pointer to the underlying
    -- representation of @a@
    --
    -- The pointer is not guaranteed to be usuable outside the scope of this
    -- function. The same warnings apply as for 'withForeignPtr'.
    withPtr :: a -> (Ptr (C a) -> IO b) -> IO b

-- | 'Nothing' is represented as a 'nullPtr'.
instance (WithPtr a) => WithPtr (Maybe a) where
    withPtr Nothing    f = f nullPtr
    withPtr (Just obj) f = withPtr obj f

-- | Mutable types use the same underlying representation as unmutable types.
instance (WithPtr a) => WithPtr (Mut a s) where
    withPtr = withPtr . unMut

--------------------------------------------------------------------------------

-- | Types of which a value can be constructed from a pointer to the C
-- equivalent of that value
--
-- Used to wrap values created in C.
class FromPtr a where
    fromPtr :: IO (Ptr (C a)) -> IO a

--------------------------------------------------------------------------------

toCFloat :: Float -> CFloat
{-# INLINE toCFloat #-}
toCFloat = CFloat

fromCFloat :: CFloat -> Float
{-# INLINE fromCFloat #-}
fromCFloat (CFloat x) = x

toCDouble :: Double -> CDouble
{-# INLINE toCDouble #-}
toCDouble = CDouble

fromCDouble :: CDouble -> Double
{-# INLINE fromCDouble #-}
fromCDouble (CDouble x) = x
