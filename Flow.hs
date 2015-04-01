module Flow (
    -- * Function application
    apply, (|>), (<|),
    -- * Function composition
    compose, (.>), (<.),
    -- * Strict function application
    apply', (!>), (<!)
) where

import Prelude (seq)

-- $setup
-- >>> import Prelude

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

{- |
    >>> apply' undefined (const False)
    *** Exception: Prelude.undefined
-}
apply' :: a -> (a -> b) -> b
apply' x f = seq x (apply x f)

{- |
    >>> undefined !> const False
    *** Exception: Prelude.undefined
-}
infixl 0 !>
(!>) :: a -> (a -> b) -> b
x !> f = apply' x f

{- |
    >>> const False <! undefined
    *** Exception: Prelude.undefined
-}
infixr 0 <!
(<!) :: (a -> b) -> a -> b
f <! x = apply' x f
