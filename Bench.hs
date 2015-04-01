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
    , bgroup "composition"
        [ bench "compose f g" $ whnf (compose f) g
        , bench "f .> g" $ whnf (f .>) g
        , bench "g <. f" $ whnf (<. f) g
        ]
    ]

x :: ()
x = ()

f :: a -> a
f = id

g :: a -> a
g = id
