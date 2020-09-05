{-# options_ghc -Wno-unused-imports #-}
module Main where

import YAGPV (draw)

-- main :: IO ()
-- main = putStrLn "hello!"

main :: IO ()
main = draw "../entity-recog/ml.prof"
