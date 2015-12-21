{-# LANGUAGE PackageImports #-}

module Main where

import "criterion" Criterion.Main
import "thea"      OpenCV
import "this"      Paths_thea ( getDataFileName )


main :: IO ()
main = defaultMain
    [ bgroup "Core"
      [
        bgroup "Mat"
        [
          bgroup "Repa"
          [ -- bench "toRepa" $ toRepa
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

-- loadImg :: IO Mat
-- loadImg = do
--     lennaFp <- getDataFileName "Lenna.png"
