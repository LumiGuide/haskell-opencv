{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module OpenCV.TypeLevel
    ( -- * Kinds and types
      DS(D, S), dsToMaybe
    , Z(Z)
    , (:::)((:::))

      -- * Type level to value level conversions
    , ToInt32(toInt32)
    , ToNatDS(toNatDS)
    , ToNatListDS(toNatListDS)

      -- * Type functions
    , Length
    , Elem
    , Relax

      -- ** Predicates (constraints)
    , In
    , MayRelax
    , All
    , IsStatic

      -- ** Type conversions
    , DSNat
    , DSNats
    ) where

import "base" Data.Int
import "base" Data.Proxy
import "base" Data.Type.Bool
import "base" GHC.Exts ( Constraint )
import "base" GHC.TypeLits

--------------------------------------------------------------------------------
-- Kinds and types
--------------------------------------------------------------------------------

-- | 'D'ynamically or 'S'tatically known values
--
-- Mainly used as a promoted type.
--
-- Operationally exactly the 'Maybe' type
data DS a
   = D   -- ^ Something is dynamically known
   | S a -- ^ Something is statically known, in particular: @a@
     deriving (Show, Eq, Functor)

-- | Converts a DS value to the corresponding Maybe value
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

class ToInt32 a where
    toInt32 :: a -> Int32

-- | value level: identity
instance ToInt32 Int32 where
    toInt32 = id

-- | type level: reify the known natural number @n@
instance (KnownNat n) => ToInt32 (proxy n) where
    toInt32 = fromInteger . natVal

--------------------------------------------------------------------------------

-- | Type level to value level conversion of numbers that are either
-- 'D'ynamically or 'S'tatically known.
--
-- > toNatDS (Proxy ('S 42)) == S 42
-- > toNatDS (Proxy 'D) == D
class ToNatDS a where
    toNatDS :: a -> DS Int32

-- | value level numbers are dynamically known
instance ToNatDS (proxy 'D) where
    toNatDS _proxy = D

-- | type level numbers are statically known
instance (ToInt32 (Proxy n)) => ToNatDS (Proxy ('S n)) where
    toNatDS _proxy = S $ toInt32 (Proxy :: Proxy n)

--------------------------------------------------------------------------------

class ToNatListDS a where
    toNatListDS :: a -> [DS Int32]

instance ToNatListDS (proxy '[]) where
    toNatListDS _proxy = []

instance (ToNatDS (Proxy a), ToNatListDS (Proxy as))
      => ToNatListDS (Proxy (a ': as)) where
    toNatListDS _proxy = (toNatDS     (Proxy :: Proxy a ))
                       : (toNatListDS (Proxy :: Proxy as))

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

type family Relax (a :: DS ka) (b :: DS kb) :: Bool where
    Relax x      'D     = 'True
    Relax ('S (x ': xs)) ('S (y ': ys)) = Relax x y && Relax ('S xs) ('S ys)
    Relax ('S x) ('S y) = Relax x y
    Relax x      x      = 'True
    Relax x      y      = 'False

type MayRelax a b = Relax a b ~ 'True

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

class PrivateIsStatic (ds :: DS a)
instance PrivateIsStatic ('S a)

class All (p :: k -> Constraint) (xs :: [k])
instance All p '[]
instance (p x, All p xs) => All p (x ': xs)

class (PrivateIsStatic ds) => IsStatic (ds :: DS a)
instance IsStatic ('S a)
