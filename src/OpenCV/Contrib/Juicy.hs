{-# language DataKinds, Rank2Types, TypeFamilies, ScopedTypeVariables, FlexibleContexts, ViewPatterns, ExistentialQuantification #-}
-- | A thin JuicyPixels layer.
module OpenCV.Contrib.Juicy (
    -- * Types
    Mat2D,
    Filter ,
    JC,
    JD,
    -- * Low level API
    fromImage,
    toImage,
    -- * High level API
    isoJuicy
    )
    where
    
import GHC.TypeLits (Nat,KnownNat)
import Data.Proxy (Proxy (Proxy))

import Foreign.Storable (Storable (..))
import Foreign.Ptr (Ptr, plusPtr)
import System.IO.Unsafe (unsafePerformIO)

import Data.Word (Word8,Word16)
import Data.Int (Int32)

import Control.Monad (forM_)
import Control.Monad.Primitive (PrimMonad)

import Linear.V4 (V4)

import OpenCV
import OpenCV.Unsafe (unsafeRead, unsafeWrite)
import Codec.Picture.Types 

-- list of pointers at a given byte distance from a base one
plusPtrS :: Ptr a -> Int -> [Ptr b]
plusPtrS p n = map (plusPtr p) [0..n-1]

-- multiple peek
peekS p n = mapM peek (plusPtrS p n)

-- multiple poke
pokeS p n xs = sequence_ (zipWith poke (plusPtrS p n) xs)

instance Storable (PixelRGB8) where
    peek p =  peekS p 3 >>= \[b,r,g] -> return (PixelRGB8 r g b)
    poke p (PixelRGB8 r g b) = pokeS p 3 [b,g,r]
    sizeOf _ = 3
    alignment _ = 0

instance Storable (PixelRGB16) where
    peek p =  peekS p 3 >>= \[b,r,g] -> return (PixelRGB16 r g b)
    poke p (PixelRGB16 r g b) = pokeS p 3 [b,g,r]
    sizeOf _ = 3
    alignment _ = 0

instance Storable (PixelRGBF) where
    peek p =  peekS p 3 >>= \[b,r,g] -> return (PixelRGBF r g b)
    poke p (PixelRGBF r g b) = pokeS p 3 [b,g,r]
    sizeOf _ = 3
    alignment _ = 0

instance Storable (PixelRGBA8) where
    peek p =  peekS p 4 >>= \[b,r,g,a] -> return (PixelRGBA8 r g b a)
    poke p (PixelRGBA8 r g b a) = pokeS p 4 [b,g,r,a]
    sizeOf _ = 4
    alignment _ = 0

instance Storable (PixelRGBA16) where
    peek p =  peekS p 4 >>= \[b,r,g,a] -> return (PixelRGBA16 r g b a)
    poke p (PixelRGBA16 r g b a) = pokeS p 4 [b,g,r,a]
    sizeOf _ = 4
    alignment _ = 0

instance Storable (PixelYA8) where
    peek p =  peekS p 2 >>= \[b,g] -> return (PixelYA8 b g)
    poke p (PixelYA8 b g) = pokeS p 2 [b,g]
    sizeOf _ = 2
    alignment _ = 0

instance Storable (PixelYA16) where
    peek p =  peekS p 2 >>= \[b,g] -> return (PixelYA16 b g)
    poke p (PixelYA16 b g) = pokeS p 2 [b,g]
    sizeOf _ = 2
    alignment _ = 0

-- | map Pixel types to a depth
type family JD a 

-- | map Pixel types to a number of channels
type family JC a :: Nat 

type instance JD Pixel8 = Word8
type instance JD Pixel16 = Word16 
type instance JD PixelF = Float
type instance JD PixelYA8 = Word8
type instance JD PixelYA16 = Word16
type instance JD PixelRGB8 = Word8
type instance JD PixelRGB16 = Word16
type instance JD PixelRGBF = Float
type instance JD PixelRGBA8 = Word8
type instance JD PixelRGBA16 = Word16
type instance JD PixelYCbCr8 = Word8
type instance JD PixelCMYK8 = Word8
type instance JD PixelCMYK16 = Word16

type instance JC Pixel8 = 1
type instance JC Pixel16 = 1
type instance JC PixelF = 1
type instance JC PixelYA8 = 2
type instance JC PixelYA16 = 2
type instance JC PixelRGB8 = 3
type instance JC PixelRGB16 = 3
type instance JC PixelRGBF = 3
type instance JC PixelRGBA8 = 4
type instance JC PixelRGBA16 = 4
type instance JC PixelYCbCr8 = 3
type instance JC PixelCMYK8 = 4
type instance JC PixelCMYK16 = 4

-- | An OpenCV bidimensional matrix
type Mat2D h w c d = Mat ('S '[h,w]) c d 

-- | Compute an OpenCV 2D-matrix from a JuicyPixels image
fromImage 
    :: forall a c d . (ToDepth (Proxy d), KnownNat c, Pixel a, Storable a, c ~ JC a, d ~ JD a) 
    => Image a -- ^ JuicyPixels image
    -> CvExcept (Mat2D 'D 'D ('S c) ('S d)) 
fromImage i@(Image h w v) = withMatM 
    (fi h ::: fi w ::: Z) 
    (Proxy :: Proxy c) 
    (Proxy :: Proxy d) 
    (pure 0 :: V4 Double) $ \m -> 
        forM_ ((,) <$> [0 .. h - 1] <*> [0 .. w - 1]) $ \(x,y) -> unsafeWrite m [y,x] (pixelAt i x y) 
  where
    fi :: Int -> Int32
    fi = fromIntegral 

-- | Compute a JuicyPixels image from an OpenCV 2D-matrix
toImage :: forall a c d . (KnownNat c, Pixel a, Storable a, c ~ JC a, d ~ JD a) 
    => Mat2D 'D 'D ('S c) ('S d)  -- ^ OpenCV 2D-matrix
    -> Image a
toImage m  = let
    MatInfo [fromIntegral -> h, fromIntegral -> w] _ _  = matInfo m
    in unsafePerformIO $ do
        mat <- unsafeThaw m
        withImage h w $ \x y -> unsafeRead mat [y,x] >>= peek
      
-- | An OpenCV 2D-filter preserving the matrix type
type Filter m h w c d = Mat2D h w c d -> CvExceptT m (Mat2D h w c d)

-- | Apply an OpenCV 2D-filter to a JuicyPixels dynamic matrix, preserving the Juicy pixel encoding
isoJuicy :: forall m c d . (PrimMonad m, ToDepthDS (Proxy d), ToNatDS (Proxy c)) 
    => Filter m 'D 'D c d -- ^ OpenCV 2D-filter
    -> DynamicImage -- ^ JuicyPixels dynamic image
    -> CvExceptT m DynamicImage 

isoJuicy f (ImageRGB8 i)    =  ImageRGB8    <$> isoApply f i 
isoJuicy f (ImageRGB16 i)   =  ImageRGB16   <$> isoApply f i 
isoJuicy f (ImageRGBF i)    =  ImageRGBF    <$> isoApply f i 
isoJuicy f (ImageY8 i)      =  ImageY8      <$> isoApply f i 
isoJuicy f (ImageY16 i)     =  ImageY16     <$> isoApply f i 
isoJuicy f (ImageRGBA8 i)   =  ImageRGBA8   <$> isoApply f i 
isoJuicy f (ImageRGBA16 i)  =  ImageRGBA16  <$> isoApply f i 

-- apply a filter for a specific 'a'. Coerce failures are lifted to the monadic layer
isoApply ::
     (PrimMonad m, KnownNat (JC a), Pixel a, Storable a, 
     ToShapeDS (Proxy ('S '[h,w])), 
     ToChannelsDS (Proxy c), 
     ToDepthDS (Proxy d), ToChannelsDS (Proxy ('S (JC a))), 
     ToDepthDS (Proxy ('S (JD a))),
     ToDepth (Proxy (JD a))
     )
  => Filter m h w c d -> Image a -> CvExceptT m (Image a)

isoApply f i = toImage <$> (pureExcept (fromImage i >>= coerceMat) >>= f >>= pureExcept . coerceMat)

