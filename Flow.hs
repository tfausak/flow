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
