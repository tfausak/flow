{- |
    Flow is a package that provides functions and operators for writing more
    understandable Haskell. It's an alternative to some common idioms like
    function application with @($)@ and function composition with @(.)@.

    Flow is designed to be imported unqualified. It does not export anything
    that conflicts with
    <http://hackage.haskell.org/package/base the base package>.

    >>> import Flow

    For more information about Flow, please visit
    <http://taylor.fausak.me/flow/ its official site>.
-}
module Flow (
    -- * Function application
    apply, (|>), (<|),
    -- * Function composition
    compose, (.>), (<.),
    -- * Strict function application
    apply', (!>), (<!),
) where

import Prelude (seq)

{- $setup
    >>> import Prelude
    >>> let f = (+ 2)
    >>> let g = (* 2)
    >>> let h = (^ 2)
-}

{- |
    prop> apply x f == f x

    <https://en.wikipedia.org/wiki/Function_application Function application>.
    This is like the 'Prelude.$' operator.

    >>> apply False not
    True

    Using this function with many arguments is cumbersome. Use '|>' or '<|'
    instead.

    >>> False `apply` not `apply` fromEnum
    1

    This function usually isn't necessary since @'apply' x f@ is the same as
    @f x@. However it can come in handy when working with higher-order
    functions.

    >>> map (apply False) [not, id]
    [True,False]
-}
apply :: a -> (a -> b) -> b
apply x f = f x

{- |
    prop> (x |> f) == f x
    prop> (x |> f |> g) == g (f x)

    Left-associative 'apply' operator. This is like a flipped version of the
    'Prelude.$' operator. Read it as "apply forward" or "pipe into".

    >>> False |> not
    True

    Since this operator has such low precedence, it can be used to remove
    parentheses from complicated expressions.

    >>> False |> not |> fromEnum
    1

    This operator can be used with higher-order functions, but 'apply' might be
    clearer.

    >>> map (False |>) [not, id]
    [True,False]
-}
infixl 1 |>
(|>) :: a -> (a -> b) -> b
x |> f = apply x f

{- |
    prop> (f <| x) == f x
    prop> (g <| f <| x) == g (f x)

    Right-associative 'apply' operator. This is like the 'Prelude.$' operator.
    Read it as "apply backward" or "pipe from".

    >>> not <| False
    True

    This operator can be used to remove parentheses from complicated
    expressions because of its low precedence.

    >>> fromEnum <| not <| False
    1

    With higher-order functions, this operator is a clearer alternative to
    @flip 'apply'@.

    >>> map (<| False) [not, id]
    [True,False]
-}
infixr 0 <|
(<|) :: (a -> b) -> a -> b
f <| x = apply x f

{- |
    prop> compose f g x == g (f x)

    <https://en.wikipedia.org/wiki/Function_composition Function composition>.
    This is like the 'Prelude..' operator.

    >>> (compose not fromEnum) False
    1

    Composing many functions together quickly becomes unwieldy. Use '.>' or
    '<.' instead.

    >>> (not `compose` fromEnum `compose` succ) False
    2
-}
compose :: (a -> b) -> (b -> c) -> (a -> c)
compose f g = \ x -> g (f x)

{- |
    prop> (f .> g) x == g (f x)
    prop> (f .> g .> h) x == h (g (f x))

    Left-associative 'compose' operator. This is like a flipped version of the
    'Prelude..' operator. Read it as "compose forward" or "and then".

    >>> (not .> fromEnum) False
    1

    Thanks to its high precedence, composing many functions together is easy.

    >>> (not .> fromEnum .> succ) False
    2
-}
infixl 9 .>
(.>) :: (a -> b) -> (b -> c) -> (a -> c)
f .> g = compose f g

{- |
    prop> (g <. f) x == g (f x)
    prop> (h <. g <. f) x == h (g (f x))

    Right-associative 'compose' operator. This is like the 'Prelude..'
    operator. Read it as "compose backward" or "but first".

    >>> (fromEnum <. not) False
    1

    Composing many functions together is easy thanks to its high precedence.

    >>> (succ <. fromEnum <. not) False
    2
-}
infixr 9 <.
(<.) :: (b -> c) -> (a -> b) -> (a -> c)
g <. f = compose f g

{- |
    prop> apply' x f == seq x (f x)

    Strict function application. This is like the 'Prelude.$!' operator.

    >>> apply' undefined (const False)
    *** Exception: Prelude.undefined
-}
apply' :: a -> (a -> b) -> b
apply' x f = seq x (apply x f)

{- |
    prop> (x !> f) == seq x (f x)
    prop> (x !> f !> g) == seq x (g (seq x (f x)))

    Left-associative 'apply'' operator. This is like a flipped version of the
    'Prelude.$!' operator.

    >>> undefined !> const False
    *** Exception: Prelude.undefined
-}
infixl 1 !>
(!>) :: a -> (a -> b) -> b
x !> f = apply' x f

{- |
    prop> (f <! x) == seq x (f x)
    prop> (g <! f <! x) == seq x (g (seq x (f x)))

    Right-associative 'apply'' operator. This is like the 'Prelude.$!'
    operator.

    >>> const False <! undefined
    *** Exception: Prelude.undefined
-}
infixr 0 <!
(<!) :: (a -> b) -> a -> b
f <! x = apply' x f
