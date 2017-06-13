{-# language DataKinds #-}
{-# language Rank2Types #-}
{-# language TypeFamilies #-}
{-# language ScopedTypeVariables #-}
{-# language FlexibleContexts #-}
{-# language ViewPatterns #-}
{-# language ExistentialQuantification #-}

-- TODO (basvandijk): upstream the Storable instances to JuicyPixels!
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | A thin JuicyPixels layer.
module OpenCV.Juicy
  ( -- * Types
    Mat2D
  , Filter
  , PixelChannels
  , PixelDepth

    -- * Low level API
  , fromImage
  , toImage

    -- * High level API
  , isoJuicy
  ) where

import "base" GHC.TypeLits (Nat,KnownNat)
import "base" Data.Proxy (Proxy (Proxy))
import "base" Foreign.Storable (Storable (..))
import "base" Foreign.Ptr (Ptr, plusPtr)
import "base" System.IO.Unsafe (unsafePerformIO)
import "base" Data.Word (Word8,Word16)
import "base" Data.Int (Int32)
import "base" Control.Monad (forM_)
import "primitive" Control.Monad.Primitive (PrimMonad)
import "linear" Linear.V4 (V4)
import "this" OpenCV
import "this" OpenCV.Unsafe (unsafeRead, unsafeWrite)
import "JuicyPixels" Codec.Picture.Types

-- list of pointers at a given byte distance from a base one
plusPtrS :: Ptr a -> Int -> [Ptr b]
plusPtrS p n = map (plusPtr p) [0..n-1]

-- multiple peek
peekS :: Storable b => Ptr a -> Int -> IO [b]
peekS p n = mapM peek (plusPtrS p n)

-- multiple poke
pokeS :: Storable a => Ptr a1 -> Int -> [a] -> IO ()
pokeS p n xs = sequence_ (zipWith poke (plusPtrS p n) xs)

instance Storable PixelRGB8 where
    peek p =  peekS p 3 >>= \[b,r,g] -> return (PixelRGB8 r g b)
    poke p (PixelRGB8 r g b) = pokeS p 3 [b,g,r]
    sizeOf _ = 3
    alignment _ = 0

instance Storable PixelRGB16 where
    peek p =  peekS p 3 >>= \[b,r,g] -> return (PixelRGB16 r g b)
    poke p (PixelRGB16 r g b) = pokeS p 3 [b,g,r]
    sizeOf _ = 3
    alignment _ = 0

instance Storable PixelRGBF where
    peek p =  peekS p 3 >>= \[b,r,g] -> return (PixelRGBF r g b)
    poke p (PixelRGBF r g b) = pokeS p 3 [b,g,r]
    sizeOf _ = 3
    alignment _ = 0

instance Storable PixelRGBA8 where
    peek p =  peekS p 4 >>= \[b,r,g,a] -> return (PixelRGBA8 r g b a)
    poke p (PixelRGBA8 r g b a) = pokeS p 4 [b,g,r,a]
    sizeOf _ = 4
    alignment _ = 0

instance Storable PixelRGBA16 where
    peek p =  peekS p 4 >>= \[b,r,g,a] -> return (PixelRGBA16 r g b a)
    poke p (PixelRGBA16 r g b a) = pokeS p 4 [b,g,r,a]
    sizeOf _ = 4
    alignment _ = 0

instance Storable PixelYA8 where
    peek p =  peekS p 2 >>= \[b,g] -> return (PixelYA8 b g)
    poke p (PixelYA8 b g) = pokeS p 2 [b,g]
    sizeOf _ = 2
    alignment _ = 0

instance Storable PixelYA16 where
    peek p =  peekS p 2 >>= \[b,g] -> return (PixelYA16 b g)
    poke p (PixelYA16 b g) = pokeS p 2 [b,g]
    sizeOf _ = 2
    alignment _ = 0

-- | map Pixel types to a depth
type family PixelDepth a

-- | map Pixel types to a number of channels
type family PixelChannels a :: Nat

type instance PixelDepth Pixel8      = Word8
type instance PixelDepth Pixel16     = Word16
type instance PixelDepth PixelF      = Float
type instance PixelDepth PixelYA8    = Word8
type instance PixelDepth PixelYA16   = Word16
type instance PixelDepth PixelRGB8   = Word8
type instance PixelDepth PixelRGB16  = Word16
type instance PixelDepth PixelRGBF   = Float
type instance PixelDepth PixelRGBA8  = Word8
type instance PixelDepth PixelRGBA16 = Word16
type instance PixelDepth PixelYCbCr8 = Word8
type instance PixelDepth PixelCMYK8  = Word8
type instance PixelDepth PixelCMYK16 = Word16

type instance PixelChannels Pixel8      = 1
type instance PixelChannels Pixel16     = 1
type instance PixelChannels PixelF      = 1
type instance PixelChannels PixelYA8    = 2
type instance PixelChannels PixelYA16   = 2
type instance PixelChannels PixelRGB8   = 3
type instance PixelChannels PixelRGB16  = 3
type instance PixelChannels PixelRGBF   = 3
type instance PixelChannels PixelRGBA8  = 4
type instance PixelChannels PixelRGBA16 = 4
type instance PixelChannels PixelYCbCr8 = 3
type instance PixelChannels PixelCMYK8  = 4
type instance PixelChannels PixelCMYK16 = 4

-- | An OpenCV bidimensional matrix
type Mat2D h w channels depth = Mat ('S '[h,w]) channels depth

{- | Compute an OpenCV 2D-matrix from a JuicyPixels image.

Example:

@
fromImageImg :: IO (Mat ('S '[ 'D, 'D]) ('S 3) ('S Word8))
fromImageImg = do
    r <- Codec.Picture.readImage "data/Lenna.png"
    case r of
      Left err -> error err
      Right (Codec.Picture.ImageRGB8 img) -> pure $ OpenCV.Juicy.fromImage img
      Right _ -> error "Unhandled JuicyPixels format!"
@

<<doc/generated/examples/fromImageImg.png fromImageImg>>
-}
fromImage
    :: forall a c d
     . ( ToDepth (Proxy d)
       , KnownNat c
       , Pixel a
       , Storable a
       , c ~ PixelChannels a
       , d ~ PixelDepth a
       )
    => Image a -- ^ JuicyPixels image
    -> Mat2D 'D 'D ('S c) ('S d)
fromImage i@(Image w h _data) = exceptError $ withMatM
    (fi h ::: fi w ::: Z)
    (Proxy :: Proxy c)
    (Proxy :: Proxy d)
    (pure 0 :: V4 Double) $ \m ->
      forM_ ((,) <$> [0 .. h - 1] <*> [0 .. w - 1]) $ \(x,y) ->
        unsafeWrite m [y,x] 0 (pixelAt i x y)
  where
    fi :: Int -> Int32
    fi = fromIntegral

{- | Compute a JuicyPixels image from an OpenCV 2D-matrix

FIXME: There's a bug in the colour conversions in the example:

Example:

@
toImageImg :: IO (Mat ('S '[ 'D, 'D]) ('S 3) ('S Word8))
toImageImg = exceptError . cvtColor rgb bgr . from . to . exceptError . cvtColor bgr rgb \<$> fromImageImg
  where
    to :: OpenCV.Juicy.Mat2D 'D 'D ('S 3) ('S Word8) -> Codec.Picture.Image Codec.Picture.PixelRGB8
    to = OpenCV.Juicy.toImage

    from :: Codec.Picture.Image Codec.Picture.PixelRGB8 -> OpenCV.Juicy.Mat2D 'D 'D ('S 3) ('S Word8)
    from = OpenCV.Juicy.fromImage
@

<<doc/generated/examples/toImageImg.png toImageImg>>
-}
toImage
    :: forall a c d h w.
       ( KnownNat c
       , Pixel a
       , Storable a
       , c ~ PixelChannels a
       , d ~ PixelDepth a
       )
    => Mat2D h w ('S c) ('S d)  -- ^ OpenCV 2D-matrix
    -> Image a
toImage m  = unsafePerformIO $ do
    mat <- unsafeThaw m
    withImage w h $ \x y -> unsafeRead mat [y, x] 0
  where
    MatInfo [fromIntegral -> h, fromIntegral -> w] _ _  = matInfo m

-- | An OpenCV 2D-filter preserving the matrix type
type Filter m h w c d = Mat2D h w c d -> CvExceptT m (Mat2D h w c d)

-- | Apply an OpenCV 2D-filter to a JuicyPixels dynamic matrix,
-- preserving the Juicy pixel encoding
isoJuicy
    :: forall m. (PrimMonad m)
    => (forall c d h w. Filter m h w c d) -- ^ OpenCV 2D-filter
    -> DynamicImage -- ^ JuicyPixels dynamic image
    -> CvExceptT m DynamicImage
isoJuicy f (ImageRGB8 i)    =  ImageRGB8    <$> isoApply f i
isoJuicy f (ImageRGB16 i)   =  ImageRGB16   <$> isoApply f i
isoJuicy f (ImageRGBF i)    =  ImageRGBF    <$> isoApply f i
isoJuicy f (ImageY8 i)      =  ImageY8      <$> isoApply f i
isoJuicy f (ImageY16 i)     =  ImageY16     <$> isoApply f i
isoJuicy f (ImageRGBA8 i)   =  ImageRGBA8   <$> isoApply f i
isoJuicy f (ImageRGBA16 i)  =  ImageRGBA16  <$> isoApply f i
isoJuicy _ _                =  error
    "Unhandled conversion from DynamicImage to Mat"

isoApply
    :: forall f inPixel outPixel
    . ( Functor f
      , KnownNat (PixelChannels inPixel)
      , KnownNat (PixelChannels outPixel)
      , Storable inPixel
      , Storable outPixel
      , ToDepth (Proxy (PixelDepth inPixel))
      , Pixel inPixel
      , Pixel outPixel
      )
    => (     Mat2D 'D 'D ('S (PixelChannels inPixel))  ('S (PixelDepth inPixel))
       -> f (Mat2D 'D 'D ('S (PixelChannels outPixel)) ('S (PixelDepth outPixel)))
       )
    -> (     Image inPixel
       -> f (Image outPixel)
       )
isoApply f = fmap toImage . f . fromImage
