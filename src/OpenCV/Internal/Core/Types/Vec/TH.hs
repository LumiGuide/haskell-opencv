{-# language CPP #-}
{-# language QuasiQuotes #-}
{-# language TemplateHaskell #-}

#ifndef ENABLE_INTERNAL_DOCUMENTATION
{-# OPTIONS_HADDOCK hide #-}
#endif

module OpenCV.Internal.Core.Types.Vec.TH
  ( mkVecType
  ) where

import "base" Data.List ( intercalate )
import "base" Data.Monoid ( (<>) )
import "base" Foreign.Marshal.Alloc ( alloca )
import "base" Foreign.Storable ( peek )
import "base" System.IO.Unsafe ( unsafePerformIO )
import qualified "inline-c" Language.C.Inline.Unsafe as CU
import "linear" Linear ( V2(..), V3(..), V4(..) )
import "template-haskell" Language.Haskell.TH
import "template-haskell" Language.Haskell.TH.Quote ( quoteExp )
import "this" OpenCV.Internal.C.PlacementNew.TH ( mkPlacementNewInstance )
import "this" OpenCV.Internal.C.Types
import "this" OpenCV.Internal.Core.Types.Vec
import "this" OpenCV.Internal

mkVecType
    :: String  -- ^ Vec type name, for both Haskell and C
    -> Integer -- ^ Vec dimension
    -> Name    -- ^ Depth type name in Haskell
    -> String  -- ^ Depth type name in C
    -> Q [Dec]
mkVecType vTypeNameStr dim depthTypeName cDepthTypeStr
    | dim < 2 || dim > 4 = fail $ "mkVecType: Unsupported dimension: " <> show dim
    | otherwise =
      fmap concat . sequence $
        [ pure <$> vecTySynD
        , fromPtrDs
        , isVecOpenCVInstanceDs
        , isVecHaskellInstanceDs
        , mkPlacementNewInstance vTypeName
        ]
  where
    vTypeName :: Name
    vTypeName = mkName vTypeNameStr

    cVecTypeStr :: String
    cVecTypeStr = vTypeNameStr

    vTypeQ :: Q Type
    vTypeQ = conT vTypeName

    depthTypeQ :: Q Type
    depthTypeQ = conT depthTypeName

    dimTypeQ :: Q Type
    dimTypeQ = litT (numTyLit dim)

    vecTySynD :: Q Dec
    vecTySynD =
        tySynD vTypeName
               []
               ([t|Vec|] `appT` dimTypeQ `appT` depthTypeQ)

    fromPtrDs :: Q [Dec]
    fromPtrDs =
        [d|
        instance FromPtr $(vTypeQ) where
          fromPtr = objFromPtr Vec $ $(finalizerExpQ)
        |]
      where
        finalizerExpQ :: Q Exp
        finalizerExpQ = do
          ptr <- newName "ptr"
          lamE [varP ptr] $
            quoteExp CU.exp $
              "void { delete $(" <> cVecTypeStr <> " * " <> nameBase ptr <> ") }"

    isVecOpenCVInstanceDs :: Q [Dec]
    isVecOpenCVInstanceDs =
        [d|
        instance IsVec (Vec $(dimTypeQ)) $(depthTypeQ) where
          toVec   = id
          toVecIO = pure
          fromVec = id
        |]

    isVecHaskellInstanceDs :: Q [Dec]
    isVecHaskellInstanceDs =
        let ix = fromInteger dim - 2
        in withLinear (linearTypeQs   !! ix)
                      (linearConNames !! ix)
      where
        linearTypeQs :: [Q Type]
        linearTypeQs = map conT [''V2, ''V3, ''V4]

        linearConNames :: [Name]
        linearConNames = ['V2, 'V3, 'V4]

        withLinear :: Q Type -> Name -> Q [Dec]
        withLinear lvTypeQ lvConName =
            [d|
            instance IsVec $(lvTypeQ) $(depthTypeQ) where
              toVec   = unsafePerformIO . toVecIO
              toVecIO = $(toVecIOExpQ)
              fromVec = $(fromVecExpQ)
            |]
          where
            toVecIOExpQ :: Q Exp
            toVecIOExpQ = do
                ns <- mapM newName elemNames
                lamE [conP lvConName $ map varP ns]
                 $ appE [e|fromPtr|]
                 $ quoteExp CU.exp
                 $ inlineCStr ns
              where
                inlineCStr :: [Name] -> String
                inlineCStr ns = concat
                    [ cVecTypeStr
                    , " * { new cv::Vec<"
                    , cDepthTypeStr
                    , ", "
                    , show dim
                    , ">(" <> intercalate ", " (map elemQuote ns) <> ")"
                    , " }"
                    ]
                  where
                    elemQuote :: Name -> String
                    elemQuote n = "$(" <> cDepthTypeStr <> " " <> nameBase n <> ")"

            fromVecExpQ :: Q Exp
            fromVecExpQ = do
                vec <- newName "vec"
                vecPtr <- newName "vecPtr"
                ptrNames <- mapM (newName . (<> "Ptr")) elemNames
                withPtrNames vec vecPtr ptrNames
              where
                withPtrNames :: Name -> Name -> [Name] -> Q Exp
                withPtrNames vec vecPtr ptrNames =
                    lamE [varP vec]
                      $ appE [e|unsafePerformIO|]
                      $ withPtrVarsExpQ ptrNames
                  where

                    withPtrVarsExpQ :: [Name] -> Q Exp
                    withPtrVarsExpQ = foldr (\p -> appE [e|alloca|] . lamE [varP p]) withAllocatedVars

                    withAllocatedVars :: Q Exp
                    withAllocatedVars =
                        appE ([e|withPtr|] `appE` varE vec)
                          $ lamE [varP vecPtr]
                          $ doE
                            [ noBindS $ quoteExp CU.block inlineCStr
                            , noBindS extractExpQ
                            ]

                    inlineCStr :: String
                    inlineCStr = unlines $
                        concat
                          [ "void {"
                          , "const cv::Vec<"
                          , cDepthTypeStr
                          , ", " <> show dim <> "> & p = *$("
                          , cVecTypeStr
                          , " * "
                          , nameBase vecPtr
                          , ");"
                          ]
                        : map ptrLine (zip [0..] ptrNames)
                        <> ["}"]
                      where
                        ptrLine :: (Int, Name) -> String
                        ptrLine (ix, ptrName) =
                            "*$(" <> cDepthTypeStr <> " * " <> nameBase ptrName <> ") = p[" <> show ix <> "];"

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
                             ["x", "y", "z", "w"]
