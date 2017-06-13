module Main where

import "base" Control.Exception ( evaluate )
import "base" Data.Monoid
import "base" Data.Word
import qualified "bytestring" Data.ByteString as B
import "criterion" Criterion.Main
import "opencv" OpenCV
import "opencv" OpenCV.Unsafe
import qualified "repa" Data.Array.Repa as Repa

main :: IO ()
main = defaultMain
    [ bgroup "Core"
      [
        bgroup "Mat"
        [
          bgroup "Repa"
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
