{-# language TypeApplications #-}

module Main where

import "base" Control.Exception ( evaluate )
import "base" Data.Word
import "base" Data.Int
import "base" Data.Proxy
import qualified "bytestring" Data.ByteString as B
import "criterion" Criterion.Main
import "linear" Linear.V4
import "opencv" OpenCV
import "opencv" OpenCV.Unsafe
import "opencv" OpenCV.Internal.Core.Types.Mat ( newMat )
import qualified "repa" Data.Array.Repa as Repa
import qualified "vector" Data.Vector as V

main :: IO ()
main = defaultMain
    [ bgroup "Core"
      [
        bgroup "Mat"
        [ bench "newMat" $ nfIO mkSimpleNewMat
        , bgroup "Repa"
          [ env (loadImgAsRepa "Lenna.png") $ \a ->
              bench "computeS" $ nfArray benchComputeS a
          , env (loadImgAsRepa "Lenna.png") $ \a ->
              bench "computeP" $ nfArrayIO (benchComputeP a)
          ]
        ]
      ]
    , bgroup "ImgProc"
      [
      ]
    , bgroup "ImgCodecs"
      [
      ]
    , bgroup "HighGui"
      [
      ]
    , bgroup "Video"
      [
      ]
    ]

mkSimpleNewMat :: IO (Mat 'D 'D ('S Word8))
mkSimpleNewMat = exceptErrorIO $
    newMat (V.fromList [1] :: V.Vector Int32)
           (1 :: Int32)
           (Proxy @Word8)
           (V4 0 0 0 0 :: V4 Double)

nfArray :: (Repa.Source r e, Repa.Shape sh) => (a -> Repa.Array r sh e) -> a -> Benchmarkable
nfArray f = nf (\x -> Repa.deepSeqArray (f x) ())

nfArrayIO :: (Repa.Source r e, Repa.Shape sh) =>  IO (Repa.Array r sh e) -> Benchmarkable
nfArrayIO m = nfIO $ do
                a <- m
                evaluate $ Repa.deepSeqArray a ()

benchComputeS :: Repa.Array (M '[ 'D, 'D ] 3) Repa.DIM3 Word8 -> Repa.Array Repa.U Repa.DIM3 Word8
benchComputeS = Repa.computeS . Repa.delay

benchComputeP :: Repa.Array (M '[ 'D, 'D ] 3) Repa.DIM3 Word8 -> IO (Repa.Array Repa.U Repa.DIM3 Word8)
benchComputeP = Repa.computeP . Repa.delay

loadImgAsRepa :: FilePath -> IO (Repa.Array (M '[ 'D, 'D ] 3) Repa.DIM3 Word8)
loadImgAsRepa fp = toRepa <$> loadImg
  where
    loadImg :: IO (Mat ('S '[ 'D, 'D ]) ('S 3) ('S Word8))
    loadImg = (unsafeCoerceMat . imdecode ImreadGrayscale) <$> B.readFile ("data/" <> fp)
