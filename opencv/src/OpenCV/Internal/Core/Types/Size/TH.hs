{-# language CPP #-}
{-# language QuasiQuotes #-}
{-# language TemplateHaskell #-}

#ifndef ENABLE_INTERNAL_DOCUMENTATION
{-# OPTIONS_HADDOCK hide #-}
#endif

module OpenCV.Internal.Core.Types.Size.TH
  ( mkSizeType
  ) where

import "base" Data.List ( intercalate )
import "base" Data.Monoid ( (<>) )
import "base" Foreign.Marshal.Alloc ( alloca )
import "base" Foreign.Storable ( peek )
import "base" System.IO.Unsafe ( unsafePerformIO )
import qualified "inline-c" Language.C.Inline.Unsafe as CU
import "linear" Linear ( V2(..) )
import "template-haskell" Language.Haskell.TH
import "template-haskell" Language.Haskell.TH.Quote ( quoteExp )
import "this" OpenCV.Internal.C.PlacementNew.TH ( mkPlacementNewInstance )
import "this" OpenCV.Internal.C.Types
import "this" OpenCV.Internal.Core.Types.Size
import "this" OpenCV.Internal

mkSizeType
    :: String  -- ^ Size type name, for both Haskell and C
    -> Name    -- ^ Depth type name in Haskell
    -> String  -- ^ Depth type name in C
    -> Q [Dec]
mkSizeType pTypeNameStr depthTypeName cDepthTypeStr =
      fmap concat . sequence $
        [ pure <$> sizeTySynD
        , fromPtrDs
        , isSizeOpenCVInstanceDs
        , isSizeHaskellInstanceDs
        , mkPlacementNewInstance pTypeName
        ]
  where
    pTypeName :: Name
    pTypeName = mkName pTypeNameStr

    cSizeTypeStr :: String
    cSizeTypeStr = pTypeNameStr

    cTemplateStr :: String
    cTemplateStr = "Size_"

    pTypeQ :: Q Type
    pTypeQ = conT pTypeName

    depthTypeQ :: Q Type
    depthTypeQ = conT depthTypeName

    sizeTySynD :: Q Dec
    sizeTySynD =
        tySynD pTypeName
               []
               ([t|Size|] `appT` depthTypeQ)

    fromPtrDs :: Q [Dec]
    fromPtrDs =
        [d|
        instance FromPtr $(pTypeQ) where
          fromPtr = objFromPtr Size $ $(finalizerExpQ)
        |]
      where
        finalizerExpQ :: Q Exp
        finalizerExpQ = do
          ptr <- newName "ptr"
          lamE [varP ptr] $
            quoteExp CU.exp $
              "void { delete $(" <> cSizeTypeStr <> " * " <> nameBase ptr <> ") }"

    isSizeOpenCVInstanceDs :: Q [Dec]
    isSizeOpenCVInstanceDs =
        [d|
        instance IsSize Size $(depthTypeQ) where
          toSize   = id
          toSizeIO = pure
          fromSize = id
        |]

    isSizeHaskellInstanceDs :: Q [Dec]
    isSizeHaskellInstanceDs =
        [d|
        instance IsSize V2 $(depthTypeQ) where
          toSize   = unsafePerformIO . toSizeIO
          toSizeIO = $(toSizeIOExpQ)
          fromSize = $(fromSizeExpQ)
        |]
      where
        toSizeIOExpQ :: Q Exp
        toSizeIOExpQ = do
            ns <- mapM newName fieldNames
            lamE [conP 'V2 $ map varP ns]
             $ appE [e|fromPtr|]
             $ quoteExp CU.exp
             $ inlineCStr ns
          where
            inlineCStr :: [Name] -> String
            inlineCStr ns = concat
                [ cSizeTypeStr
                , " * { new cv::" <> cTemplateStr
                , "<" <> cDepthTypeStr <> ">"
                , "(" <> intercalate ", " (map fieldQuote ns) <> ")"
                , " }"
                ]
              where
                fieldQuote :: Name -> String
                fieldQuote n = "$(" <> cDepthTypeStr <> " " <> nameBase n <> ")"

        fromSizeExpQ :: Q Exp
        fromSizeExpQ = do
            size     <- newName "size"
            sizePtr  <- newName "sizePtr"
            ptrNames <- mapM (newName . (<> "Ptr")) fieldNames
            withPtrNames size sizePtr ptrNames
          where
            withPtrNames :: Name -> Name -> [Name] -> Q Exp
            withPtrNames size sizePtr ptrNames =
                lamE [varP size]
                  $ appE [e|unsafePerformIO|]
                  $ withPtrVarsExpQ ptrNames
              where
                withPtrVarsExpQ :: [Name] -> Q Exp
                withPtrVarsExpQ = foldr (\p -> appE [e|alloca|] . lamE [varP p]) withAllocatedVars

                withAllocatedVars :: Q Exp
                withAllocatedVars =
                    appE ([e|withPtr|] `appE` varE size)
                      $ lamE [varP sizePtr]
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
                      , cSizeTypeStr
                      , " * "
                      , nameBase sizePtr
                      , ");"
                      ]
                    : map ptrLine (zip fieldNames ptrNames)
                    <> ["}"]
                  where
                    ptrLine :: (String, Name) -> String
                    ptrLine (fieldName, ptrName) =
                        "*$(" <> cDepthTypeStr <> " * " <> nameBase ptrName <> ") = p." <> fieldName <> ";"

                extractExpQ :: Q Exp
                extractExpQ = foldl (\acc peekExp -> [e|(<*>)|] `appE` acc `appE` peekExp)
                                    ([e|pure V2|])
                                    peekExpQs
                  where
                    peekExpQs :: [Q Exp]
                    peekExpQs = map (\p -> [e|peek|] `appE` varE p) ptrNames

        fieldNames :: [String]
        fieldNames = ["width", "height"]
