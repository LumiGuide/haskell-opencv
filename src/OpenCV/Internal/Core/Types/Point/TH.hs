{-# language CPP #-}
{-# language QuasiQuotes #-}
{-# language TemplateHaskell #-}

#ifndef ENABLE_INTERNAL_DOCUMENTATION
{-# OPTIONS_HADDOCK hide #-}
#endif

module OpenCV.Internal.Core.Types.Point.TH
  ( mkPointType
  ) where

import "base" Data.List ( intercalate )
import "base" Data.Monoid ( (<>) )
import "base" Foreign.Marshal.Alloc ( alloca )
import "base" Foreign.Storable ( peek )
import "base" System.IO.Unsafe ( unsafePerformIO )
import qualified "inline-c" Language.C.Inline.Unsafe as CU
import "linear" Linear ( V2(..), V3(..) )
import "template-haskell" Language.Haskell.TH
import "template-haskell" Language.Haskell.TH.Quote ( quoteExp )
import "this" OpenCV.Internal.C.PlacementNew.TH ( mkPlacementNewInstance )
import "this" OpenCV.Internal.C.Types
import "this" OpenCV.Internal.Core.Types.Point
import "this" OpenCV.Internal

mkPointType
    :: String  -- ^ Point type name, for both Haskell and C
    -> Integer -- ^ Point dimension
    -> String  -- ^ Point template name in C
    -> Name    -- ^ Depth type name in Haskell
    -> String  -- ^ Depth type name in C
    -> Q [Dec]
mkPointType pTypeNameStr dim cTemplateStr depthTypeName cDepthTypeStr
    | dim < 2 || dim > 3 = fail $ "mkPointType: Unsupported dimension: " <> show dim
    | otherwise =
      fmap concat . sequence $
        [ pure <$> pointTySynD
        , fromPtrDs
        , isPointOpenCVInstanceDs
        , isPointHaskellInstanceDs
        , mkPlacementNewInstance pTypeName
        ]
  where
    pTypeName :: Name
    pTypeName = mkName pTypeNameStr

    cPointTypeStr :: String
    cPointTypeStr = pTypeNameStr

    pTypeQ :: Q Type
    pTypeQ = conT pTypeName

    depthTypeQ :: Q Type
    depthTypeQ = conT depthTypeName

    dimTypeQ :: Q Type
    dimTypeQ = litT (numTyLit dim)

    pointTySynD :: Q Dec
    pointTySynD =
        tySynD pTypeName
               []
               ([t|Point|] `appT` dimTypeQ `appT` depthTypeQ)

    fromPtrDs :: Q [Dec]
    fromPtrDs =
        [d|
        instance FromPtr $(pTypeQ) where
          fromPtr = objFromPtr Point $ $(finalizerExpQ)
        |]
      where
        finalizerExpQ :: Q Exp
        finalizerExpQ = do
          ptr <- newName "ptr"
          lamE [varP ptr] $
            quoteExp CU.exp $
              "void { delete $(" <> cPointTypeStr <> " * " <> nameBase ptr <> ") }"

    isPointOpenCVInstanceDs :: Q [Dec]
    isPointOpenCVInstanceDs =
        [d|
        instance IsPoint (Point $(dimTypeQ)) $(depthTypeQ) where
          toPoint   = id
          toPointIO = pure
          fromPoint = id
        |]

    isPointHaskellInstanceDs :: Q [Dec]
    isPointHaskellInstanceDs =
        let ix = fromInteger dim - 2
        in withLinear (linearTypeQs   !! ix)
                      (linearConNames !! ix)
      where
        linearTypeQs :: [Q Type]
        linearTypeQs = map conT [''V2, ''V3]

        linearConNames :: [Name]
        linearConNames = ['V2, 'V3]

        withLinear :: Q Type -> Name -> Q [Dec]
        withLinear lpTypeQ lvConName =
            [d|
            instance IsPoint $(lpTypeQ) $(depthTypeQ) where
              toPoint   = unsafePerformIO . toPointIO
              toPointIO = $(toPointIOExpQ)
              fromPoint = $(fromPointExpQ)
            |]
          where
            toPointIOExpQ :: Q Exp
            toPointIOExpQ = do
                ns <- mapM newName elemNames
                lamE [conP lvConName $ map varP ns]
                 $ appE [e|fromPtr|]
                 $ quoteExp CU.exp
                 $ inlineCStr ns
              where
                inlineCStr :: [Name] -> String
                inlineCStr ns = concat
                    [ cPointTypeStr
                    , " * { new cv::" <> cTemplateStr
                    , "<" <> cDepthTypeStr <> ">"
                    , "(" <> intercalate ", " (map elemQuote ns) <> ")"
                    , " }"
                    ]
                  where
                    elemQuote :: Name -> String
                    elemQuote n = "$(" <> cDepthTypeStr <> " " <> nameBase n <> ")"

            fromPointExpQ :: Q Exp
            fromPointExpQ = do
                point <- newName "point"
                pointPtr <- newName "pointPtr"
                ptrNames <- mapM (newName . (<> "Ptr")) elemNames
                withPtrNames point pointPtr ptrNames
              where
                withPtrNames :: Name -> Name -> [Name] -> Q Exp
                withPtrNames point pointPtr ptrNames =
                    lamE [varP point]
                      $ appE [e|unsafePerformIO|]
                      $ withPtrVarsExpQ ptrNames
                  where

                    withPtrVarsExpQ :: [Name] -> Q Exp
                    withPtrVarsExpQ = foldr (\p -> appE [e|alloca|] . lamE [varP p]) withAllocatedVars

                    withAllocatedVars :: Q Exp
                    withAllocatedVars =
                        appE ([e|withPtr|] `appE` varE point)
                          $ lamE [varP pointPtr]
                          $ doE
                            [ noBindS $ quoteExp CU.block inlineCStr
                            , noBindS extractExpQ
                            ]

                    inlineCStr :: String
                    inlineCStr = unlines $
                        concat
                          [ "void {"
                          , "const cv::" <> cTemplateStr
                          , "<" <> cDepthTypeStr <> ">"
                          , " & p = *$("
                          , cPointTypeStr
                          , " * "
                          , nameBase pointPtr
                          , ");"
                          ]
                        : map ptrLine (zip [0..] ptrNames)
                        <> ["}"]
                      where
                        ptrLine :: (Int, Name) -> String
                        ptrLine (ix, ptrName) =
                            "*$(" <> cDepthTypeStr <> " * " <> nameBase ptrName <> ") = p." <> elemNames !! ix <> ";"

                    -- Applies the constructor to the values that are
                    -- read from the pointers.
                    extractExpQ :: Q Exp
                    extractExpQ = foldl (\acc peekExp -> [e|(<*>)|] `appE` acc `appE` peekExp)
                                        ([e|pure|] `appE` conE lvConName)
                                        peekExpQs
                      where
                        peekExpQs :: [Q Exp]
                        peekExpQs = map (\p -> [e|peek|] `appE` varE p) ptrNames

            elemNames :: [String]
            elemNames = take (fromInteger dim)
                             ["x", "y", "z"]
