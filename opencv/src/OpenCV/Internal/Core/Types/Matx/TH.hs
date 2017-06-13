{-# language CPP #-}
{-# language QuasiQuotes #-}
{-# language TemplateHaskell #-}

#ifndef ENABLE_INTERNAL_DOCUMENTATION
{-# OPTIONS_HADDOCK hide #-}
#endif

module OpenCV.Internal.Core.Types.Matx.TH
  ( mkMatxType
  ) where

import "base" Data.List ( intercalate )
import "base" Data.Monoid ( (<>) )
import qualified "inline-c" Language.C.Inline.Unsafe as CU
import "template-haskell" Language.Haskell.TH
import "template-haskell" Language.Haskell.TH.Quote ( quoteExp )
import "this" OpenCV.Internal
import "this" OpenCV.Internal.Core.Types.Matx
import "this" OpenCV.Internal.C.PlacementNew.TH ( mkPlacementNewInstance )
import "this" OpenCV.Internal.C.Types

mkMatxType
    :: String  -- ^ Matx type name, for both Haskell and C
    -> Integer -- ^ Row dimension
    -> Integer -- ^ Column dimension
    -> Name    -- ^ Depth type name in Haskell
    -> String  -- ^ Depth type name in C
    -> Q [Dec]
mkMatxType mTypeNameStr dimR dimC depthTypeName cDepthTypeStr
    | dimR < 1 || dimR > 6 || dimC < 1 || dimC > 6 =
        fail $ "mkMatxType: Unsupported dimension: " <> show dimR <> "x" <> show dimC
    | otherwise =
      fmap concat . sequence $
        [ (:[]) <$> matxTySynD
        , fromPtrDs
        , isMatxOpenCVInstanceDs
          -- The largest Matx constructor in C++ takes 16 arguments.
          -- TODO (RvD): for larger number of arguments we can use the
          -- constructor that initializes from a plain array.
        , if dimR * dimC <= 16
          then newMatxDs
          else pure []
        , mkPlacementNewInstance mTypeName
        ]
  where
    mTypeName :: Name
    mTypeName = mkName mTypeNameStr

    cMatxTypeStr :: String
    cMatxTypeStr = mTypeNameStr

    mTypeQ :: Q Type
    mTypeQ = conT mTypeName

    depthTypeQ :: Q Type
    depthTypeQ = conT depthTypeName

    dimRTypeQ, dimCTypeQ :: Q Type
    dimRTypeQ = litT (numTyLit dimR)
    dimCTypeQ = litT (numTyLit dimC)

    matxTySynD :: Q Dec
    matxTySynD =
        tySynD mTypeName
               []
               ([t|Matx|] `appT` dimRTypeQ `appT` dimCTypeQ `appT` depthTypeQ)

    fromPtrDs :: Q [Dec]
    fromPtrDs =
        [d|
        instance FromPtr $(mTypeQ) where
          fromPtr = objFromPtr Matx $ $(finalizerExpQ)
        |]
      where
        finalizerExpQ :: Q Exp
        finalizerExpQ = do
          ptr <- newName "ptr"
          lamE [varP ptr] $
            quoteExp CU.exp $
              "void { delete $(" <> cMatxTypeStr <> " * " <> nameBase ptr <> ") }"

    isMatxOpenCVInstanceDs :: Q [Dec]
    isMatxOpenCVInstanceDs =
        [d|
        instance IsMatx (Matx $(dimRTypeQ) $(dimCTypeQ)) $(depthTypeQ) where
          toMatx   = id
          toMatxIO = pure
          fromMatx = id
        |]

    newMatxDs :: Q [Dec]
    newMatxDs = sequence
        [ sigD funName funTypeQ
        , withVarNames =<< mapM newName fieldNames
        ]
      where
        -- example: Float -> Float -> Float -> Float -> IO Matx22f
        funTypeQ :: Q Type
        funTypeQ = foldr (\_fieldName acc -> arrowT `appT` depthTypeQ `appT` acc)
                         ([t|IO|] `appT` mTypeQ)
                         fieldNames

        funName :: Name
        funName = mkName $ "new" <> mTypeNameStr

        fieldNames :: [String]
        fieldNames = [fieldName r c | r <- [1..dimR], c <- [1..dimC]]
          where
            fieldName :: Integer -> Integer -> String
            fieldName r c = "f" <> show r <> show c

        withVarNames :: [Name] -> Q Dec
        withVarNames varNames = funD funName [funClause]
          where
            funClause :: Q Clause
            funClause = clause (map varP varNames) funBody []

            funBody :: Q Body
            funBody = normalB $ appE [e|fromPtr|] $ quoteExp CU.exp $ concat
                        [ cMatxTypeStr
                        , " * { new cv::Matx<"
                        , cDepthTypeStr
                        , ", "
                        , show dimR
                        , ", "
                        , show dimC
                        , ">"
                        , "("
                        , intercalate ", " (map fieldQuote varNames)
                        , ")"
                        , "}"
                        ]

            fieldQuote :: Name -> String
            fieldQuote n = "$(" <> cDepthTypeStr <> " " <> nameBase n <> ")"
