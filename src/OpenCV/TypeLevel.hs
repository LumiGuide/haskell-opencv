{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module OpenCV.TypeLevel
    ( -- * Kinds and types
      DS(D, S), dsToMaybe
    , Z(Z)
    , (:::)((:::))

      -- * Type level to value level conversions
    , Convert(convert)

      -- * Type functions
    , Length
    , Elem
      -- ** Predicates (constraints)
    , In
      -- ** Type conversions
    , DSNat
    , DSNats
    , ChannelsT
    , ShapeT
    ) where

import "base" Data.Int
import "base" Data.Proxy
import "base" GHC.TypeLits
import qualified "vector" Data.Vector as V

--------------------------------------------------------------------------------
-- Kinds and types
--------------------------------------------------------------------------------

-- | 'D'ynamically or 'S'tatically known values
--
-- Operationally exactly the 'Maybe' type
data DS a = D | S a deriving (Show, Eq, Functor)

dsToMaybe :: DS a -> Maybe a
dsToMaybe D     = Nothing
dsToMaybe (S a) = Just a

-- | End of list
data Z = Z

-- | Heterogeneous lists
--
-- Implemented as nested 2-tuples.
--
-- > f :: Int ::: Bool ::: Char ::: Z
-- > f = 3 ::: False ::: 'X' ::: Z
data a ::: b = a ::: b

infixr 5 :::


--------------------------------------------------------------------------------
-- Type level to value level conversions
--------------------------------------------------------------------------------

class Convert a b where
    convert :: a -> b

instance Convert (a :: *) (a :: *) where
    convert = id

instance (Convert a b) => Convert [a] [b] where
    convert = map convert

instance (KnownNat n) => Convert (Proxy n) Integer where
    convert = natVal

instance (KnownNat n) => Convert (Proxy n) Int32 where
    convert = fromInteger . convert

instance Convert Z [b] where
    convert Z = []

instance (Convert a b, Convert as [b]) => Convert (a ::: as) [b] where
    convert (a ::: as) = convert a : convert as

instance (Convert (a ::: as) [b]) => Convert (a ::: as) (V.Vector b) where
    convert = V.fromList . convert

instance Convert (Proxy '[]) [b] where
    convert _proxy = []

instance ( Convert (Proxy a )  b
         , Convert (Proxy as) [b]
         )
      => Convert (Proxy (a ': as)) [b] where
    convert _proxy = convert (Proxy :: Proxy a) : convert (Proxy :: Proxy as)

instance (Convert (Proxy a) b, Convert (Proxy as) [b])
      => Convert (Proxy (a ': as)) (V.Vector b) where
    convert = V.fromList . convert

instance Convert (Proxy 'D) (DS b) where
    convert _proxy = D

instance (Convert (Proxy a) b)
      => Convert (Proxy ('S a)) (DS b) where
    convert _proxy = S $ convert (Proxy :: Proxy a)


--------------------------------------------------------------------------------
-- Type functions
--------------------------------------------------------------------------------

type family Length (xs :: [a]) :: Nat where
    Length '[]        = 0
    Length (_x ': xs) = 1 + Length xs

type family Elem (e :: a) (xs :: [a]) :: Bool where
    Elem _e '[]         = 'False
    Elem  e (e  ': _xs) = 'True
    Elem  e (_x ':  xs) = Elem e xs

type In e xs = Elem e xs ~ 'True

type family DSNat (a :: ka) :: DS Nat where
    DSNat Integer    = 'D
    DSNat Int32      = 'D
    DSNat (Proxy n)  = 'S n
    DSNat (n :: Nat) = 'S n

type family DSNats (a :: ka) :: [DS Nat] where
    DSNats Z          = '[]
    DSNats (x ::: xs) = DSNat x ': DSNats xs

    DSNats ('[] :: [Nat]) = '[]
    DSNats (x ': xs)      = DSNat x ': DSNats xs

type ChannelsT a = DSNat a

type family ShapeT (a :: ka) :: DS [DS Nat] where
    ShapeT [Int32]          = 'D
    ShapeT (V.Vector Int32) = 'D
    ShapeT (x ::: xs)       = 'S (DSNats (x ::: xs))
    ShapeT (xs :: [Nat])    = 'S (DSNats xs)
    ShapeT (Proxy a)        = ShapeT a


-- type family LeDS_F (a :: Nat) (b :: DS Nat) :: Bool where
--     LeDS_F _a 'D     = 'True
--     LeDS_F  a ('S b) = a <=? b

-- type (.<=?) a b = LeDS_F a b ~ 'True

-- type LE a b = a <=? b ~ True
-- type GT a b = b <=? a ~ True


-- type family LengthDS (as :: DS [k]) :: DS Nat where
--     LengthDS 'D = 'D
--     LengthDS ('S xs) = 'S (Length xs)

-- type family MinLengthDS_F (a :: Nat) (bs :: DS [k]) :: Bool where
--     MinLengthDS_F _a 'D = 'True
--     MinLengthDS_F  a bs = LeDS_F a (LengthDS bs)

-- type MinLengthDS a bs = MinLengthDS_F a bs ~ 'True
