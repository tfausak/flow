{- |
    Flow provides operators for writing more understandable Haskell. It is an
    alternative to some common idioms like ('Prelude.$') for function
    application and ('Prelude..') for function composition.

    Flow is designed to be imported unqualified. It does not export anything
    that conflicts with the base package.

    >>> import Flow
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

    Left-associative 'apply' operator. Read as "apply forward" or "pipe into".
    Use this to create long chains of computation that suggest which direction
    things move in.

    >>> 3 |> succ |> recip |> negate
    -0.25

    Or use it anywhere you would use ('Prelude.&').
-}
infixl 0 |>
(|>) :: a -> (a -> b) -> b
x |> f = apply x f

{- |
    prop> (f <| x) == f x

    prop> (g <| f <| x) == g (f x)

    Right-associative 'apply' operator. Read as "apply backward" or "pipe
    from". Use this to create long chains of computation that suggest which
    direction things move in. You may prefer this operator over ('|>') for
    'Prelude.IO' actions since it puts the last function first.

    >>> print <| negate <| recip <| succ <| 3
    -0.25

    Or use it anywhere you would use ('Prelude.$').
-}
infixr 0 <|
(<|) :: (a -> b) -> a -> b
f <| x = apply x f

{- |
    prop> apply x f == f x

    Function application. This function usually isn't necessary, but it can be
    more readable than some alternatives when used with higher-order functions
    like 'Prelude.map'.

    >>> map (apply 2) [succ, recip, negate]
    [3.0,0.5,-2.0]
-}
apply :: a -> (a -> b) -> b
apply x f = f x

{- |
    prop> (f .> g) x == g (f x)

    prop> (f .> g .> h) x == h (g (f x))

    Left-associative 'compose' operator. Read as "compose forward" or "and
    then". Use this to create long chains of computation that suggest which
    direction things move in.

    >>> let f = succ .> recip .> negate
    >>> f 3
    -0.25

    Or use it anywhere you would use ('Control.Category.>>>').
-}
infixl 9 .>
(.>) :: (a -> b) -> (b -> c) -> (a -> c)
f .> g = compose f g

{- |
    prop> (g <. f) x == g (f x)

    prop> (h <. g <. f) x == h (g (f x))

    Right-associative 'compose' operator. Read as "compose backward" or "but
    first". Use this to create long chains of computation that suggest which
    direction things move in. You may prefer this operator over ('.>') for
    'Prelude.IO' actions since it puts the last function first.

    >>> let f = print <. negate <. recip <. succ
    >>> f 3
    -0.25

    Or use it anywhere you would use ('Prelude..').
-}
infixr 9 <.
(<.) :: (b -> c) -> (a -> b) -> (a -> c)
g <. f = compose f g

{- |
    prop> compose f g x == g (f x)

    Function composition. This function usually isn't necessary, but it can be
    more readable than some alternatives when used with higher-order functions
    like 'Prelude.map'.

    >>> let fs = map (compose succ) [recip, negate]
    >>> map (apply 3) fs
    [0.25,-4.0]
-}
compose :: (a -> b) -> (b -> c) -> (a -> c)
compose f g = \ x -> g (f x)

{- |
    prop> (x !> f) == seq x (f x)

    prop> (x !> f !> g) == let y = seq x (f x) in seq y (g y)

    Left-associative 'apply'' operator. Read as "strict apply forward" or
    "strict pipe info". Use this to create long chains of computation that
    suggest which direction things move in.

    >>> 3 !> succ !> recip !> negate
    -0.25

    The difference between this and ('|>') is that this evaluates its argument
    before passing it to the function.

    >>> undefined |> const True
    True
    >>> undefined !> const True
    *** Exception: Prelude.undefined
-}
infixl 0 !>
(!>) :: a -> (a -> b) -> b
x !> f = apply' x f

{- |
    prop> (f <! x) == seq x (f x)

    prop> (g <! f <! x) == let y = seq x (f x) in seq y (g y)

    Right-associative 'apply'' operator. Read as "strict apply backward" or
    "strict pipe from". Use this to create long chains of computation that
    suggest which direction things move in. You may prefer this operator over
    ('!>') for 'Prelude.IO' actions since it puts the last function first.

    >>> print <! negate <! recip <! succ <! 3
    -0.25

    The difference between this and ('<|') is that this evaluates its argument
    before passing it to the function.

    >>> const True <| undefined
    True
    >>> const True <! undefined
    *** Exception: Prelude.undefined
-}
infixr 0 <!
(<!) :: (a -> b) -> a -> b
f <! x = apply' x f

{- |
    prop> apply' x f == seq x (f x)

    Strict function application. This function usually isn't necessary, but it
    can be more readable than some alternatives when used with higher-order
    functions like 'Prelude.map'.

    >>> map (apply' 2) [succ, recip, negate]
    [3.0,0.5,-2.0]

    The different between this and 'apply' is that this evaluates its argument
    before passing it to the function.

    >>> apply undefined (const True)
    True
    >>> apply' undefined (const True)
    *** Exception: Prelude.undefined
-}
apply' :: a -> (a -> b) -> b
apply' x f = seq x (apply x f)
