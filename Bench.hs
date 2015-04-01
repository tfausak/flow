module Main (main) where

import Criterion.Main
import Flow

main :: IO ()
main = defaultMain
    [ bgroup "application"
        [ bench "apply x f" $ whnf (apply x) f
        , bench "x |> f" $ whnf (x |>) f
        , bench "f <| x" $ whnf (<| x) f
        ]
    ]

x :: ()
x = ()

f :: a -> a
f = id
