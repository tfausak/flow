{- |
-}
module Flow (
    -- * Function application
    (|>), (<|), apply,
    -- * Function composition
    (.>), (<.), compose,
    -- * Strict function application
    (!>), (<!), apply',
) where

import Prelude (seq)

{- $setup
    >>> import Prelude
    >>> let f = (+ 3)
    >>> let g = (* 3)
    >>> let h = (^ 3)
-}

{- |
    prop> (x |> f) == f x
    prop> (x |> f |> g) == g (f x)
-}
infixl 0 |>
(|>) :: a -> (a -> b) -> b
x |> f = apply x f

{- |
    prop> (f <| x) == f x
    prop> (g <| f <| x) == g (f x)
-}
infixr 0 <|
(<|) :: (a -> b) -> a -> b
f <| x = apply x f

{- |
    prop> apply x f == f x
-}
apply :: a -> (a -> b) -> b
apply x f = f x

{- |
    prop> (f .> g) x == g (f x)
    prop> (f .> g .> h) x == h (g (f x))
-}
infixl 9 .>
(.>) :: (a -> b) -> (b -> c) -> (a -> c)
f .> g = compose f g

{- |
    prop> (g <. f) x == g (f x)
    prop> (h <. g <. f) x == h (g (f x))
-}
infixr 9 <.
(<.) :: (b -> c) -> (a -> b) -> (a -> c)
g <. f = compose f g

{- |
    prop> compose f g x == g (f x)
-}
compose :: (a -> b) -> (b -> c) -> (a -> c)
compose f g = \ x -> g (f x)

{- |
    prop> (x !> f) == seq x (f x)
    prop> (x !> f !> g) == seq x (g (seq x (f x)))
-}
infixl 0 !>
(!>) :: a -> (a -> b) -> b
x !> f = apply' x f

{- |
    prop> (f <! x) == seq x (f x)
    prop> (g <! f <! x) == seq x (g (seq x (f x)))
-}
infixr 0 <!
(<!) :: (a -> b) -> a -> b
f <! x = apply' x f

{- |
    prop> apply' x f == seq x (f x)
-}
apply' :: a -> (a -> b) -> b
apply' x f = seq x (apply x f)
