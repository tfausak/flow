module Flow where

-- * Function application

{- |
    >>> apply False not
    True
-}
apply :: a -> (a -> b) -> b
apply x f = f x

{- |
    >>> False |> not
    True
-}
infixl 0 |>
(|>) :: a -> (a -> b) -> b
x |> f = apply x f

{- |
    >>> not <| False
    True
-}
infixr 0 <|
(<|) :: (a -> b) -> a -> b
f <| x = apply x f

-- * Function composition

{- |
    >>> (compose not fromEnum) False
    1
-}
compose :: (a -> b) -> (b -> c) -> (a -> c)
compose f g = \ x -> g (f x)

{- |
    >>> (not .> fromEnum) False
    1
-}
infixl 9 .>
(.>) :: (a -> b) -> (b -> c) -> (a -> c)
f .> g = compose f g

{- |
    >>> (fromEnum <. not) False
    1
-}
infixr 9 <.
(<.) :: (b -> c) -> (a -> b) -> (a -> c)
g <. f = compose f g
