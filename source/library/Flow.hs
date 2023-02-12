-- | Flow provides operators for writing more understandable Haskell. It is an
-- alternative to some common idioms like ('Prelude.$') for function
-- application and ('Prelude..') for function composition.
--
-- Flow is designed to be imported unqualified. It does not export anything
-- that conflicts with the base package.
--
-- >>> import Flow
--
-- == Rationale
--
-- I think that Haskell can be hard to read. It has two operators for applying
-- functions. Both are not really necessary and only serve to reduce
-- parentheses. But they make code hard to read. People who do not already
-- know Haskell have no chance of guessing what @foo $ bar@ or @baz & qux@
-- mean.
--
-- Those that do know Haskell are forced to read lines forwards and backwards
-- at the same time, thanks to function composition. Even something simple,
-- like finding the minimum element, bounces around: @f = head . sort@.
--
-- I think we can do better. By using directional operators, we can allow
-- readers to move their eye in only one direction, be that left-to-right or
-- right-to-left. And by using idioms common in other programming languages,
-- we can allow people who aren't familiar with Haskell to guess at the
-- meaning.
--
-- So instead of ('Prelude.$'), I propose ('<|'). It is a pipe, which anyone
-- who has touched a Unix system should be familiar with. And it points in the
-- direction it sends arguments along. Similarly, replace ('Prelude.&') with
-- ('|>'). And for composition, ('<.') replaces ('Prelude..'). I would have
-- preferred @<<@, but its counterpart @>>@ is taken by Haskell's syntax.
-- So-called "backwards" composition is normally expressed with
-- ('Control.Category.>>>'), which Flow provides as ('.>').
module Flow
  ( -- * Function application
    (|>),
    (<|),
    apply,

    -- * Function composition
    (.>),
    (<.),
    compose,

    -- * Strict function application
    (!>),
    (<!),
    apply',
  )
where

import Prelude (seq)

-- | Left-associative 'apply' operator. Read as "apply forward" or "pipe into".
-- Use this to create long chains of computation that suggest which direction
-- things move in.
--
-- >>> 3 |> succ |> recip |> negate
-- -0.25
--
-- Or use it anywhere you would use ('Prelude.&').
--
-- prop> \ x -> (x |> f) == f x
--
-- prop> \ x -> (x |> f |> g) == g (f x)
infixl 0 |>

(|>) :: a -> (a -> b) -> b
x |> f = apply x f

-- | Right-associative 'apply' operator. Read as "apply backward" or "pipe
-- from". Use this to create long chains of computation that suggest which
-- direction things move in. You may prefer this operator over ('|>') for
-- 'Prelude.IO' actions since it puts the last function first.
--
-- >>> print <| negate <| recip <| succ <| 3
-- -0.25
--
-- Or use it anywhere you would use ('Prelude.$').
--
-- Note that ('<|') and ('|>') have the same precedence, so they cannot be used
-- together.
--
-- >>> -- This doesn't work!
-- >>> -- print <| 3 |> succ |> recip |> negate
--
-- prop> \ x -> (f <| x) == f x
--
-- prop> \ x -> (g <| f <| x) == g (f x)
infixr 0 <|

(<|) :: (a -> b) -> a -> b
f <| x = apply x f

-- | Function application. This function usually isn't necessary, but it can be
-- more readable than some alternatives when used with higher-order functions
-- like 'Prelude.map'.
--
-- >>> map (apply 2) [succ, recip, negate]
-- [3.0,0.5,-2.0]
--
-- In general you should prefer using an explicit lambda or operator section.
--
-- >>> map (\ f -> 2 |> f) [succ, recip, negate]
-- [3.0,0.5,-2.0]
-- >>> map (2 |>) [succ, recip, negate]
-- [3.0,0.5,-2.0]
-- >>> map (<| 2) [succ, recip, negate]
-- [3.0,0.5,-2.0]
--
-- prop> \ x -> apply x f == f x
apply :: a -> (a -> b) -> b
apply x f = f x

-- | Left-associative 'compose' operator. Read as "compose forward" or "and
-- then". Use this to create long chains of computation that suggest which
-- direction things move in.
--
-- >>> let f = succ .> recip .> negate
-- >>> f 3
-- -0.25
--
-- Or use it anywhere you would use ('Control.Category.>>>').
--
-- prop> \ x -> (f .> g) x == g (f x)
--
-- prop> \ x -> (f .> g .> h) x == h (g (f x))
infixl 9 .>

(.>) :: (a -> b) -> (b -> c) -> (a -> c)
f .> g = compose f g

-- | Right-associative 'compose' operator. Read as "compose backward" or "but
-- first". Use this to create long chains of computation that suggest which
-- direction things move in. You may prefer this operator over ('.>') for
-- 'Prelude.IO' actions since it puts the last function first.
--
-- >>> let f = print <. negate <. recip <. succ
-- >>> f 3
-- -0.25
--
-- Or use it anywhere you would use ('Prelude..').
--
-- Note that ('<.') and ('.>') have the same precedence, so they cannot be used
-- together.
--
-- >>> -- This doesn't work!
-- >>> -- print <. succ .> recip .> negate
--
-- prop> \ x -> (g <. f) x == g (f x)
--
-- prop> \ x -> (h <. g <. f) x == h (g (f x))
infixr 9 <.

(<.) :: (b -> c) -> (a -> b) -> (a -> c)
g <. f = compose f g

-- | Function composition. This function usually isn't necessary, but it can be
-- more readable than some alternatives when used with higher-order functions
-- like 'Prelude.map'.
--
-- >>> let fs = map (compose succ) [recip, negate]
-- >>> map (apply 3) fs
-- [0.25,-4.0]
--
-- In general you should prefer using an explicit lambda or operator section.
--
-- >>> map (\ f -> f 3) (map (\ f -> succ .> f) [recip, negate])
-- [0.25,-4.0]
-- >>> map (\ f -> f 3) (map (succ .>) [recip, negate])
-- [0.25,-4.0]
-- >>> map (\ f -> f 3) (map (<. succ) [recip, negate])
-- [0.25,-4.0]
--
-- prop> \ x -> compose f g x == g (f x)
compose :: (a -> b) -> (b -> c) -> (a -> c)
compose f g x = g (f x)

-- | Left-associative 'apply'' operator. Read as "strict apply forward" or
-- "strict pipe into". Use this to create long chains of computation that
-- suggest which direction things move in.
--
-- >>> 3 !> succ !> recip !> negate
-- -0.25
--
-- The difference between this and ('|>') is that this evaluates its argument
-- before passing it to the function.
--
-- >>> undefined |> const True
-- True
-- >>> undefined !> const True
-- *** Exception: Prelude.undefined
-- ...
--
-- prop> \ x -> (x !> f) == seq x (f x)
--
-- prop> \ x -> (x !> f !> g) == let y = seq x (f x) in seq y (g y)
infixl 0 !>

(!>) :: a -> (a -> b) -> b
x !> f = apply' x f

-- | Right-associative 'apply'' operator. Read as "strict apply backward" or
-- "strict pipe from". Use this to create long chains of computation that
-- suggest which direction things move in. You may prefer this operator over
-- ('!>') for 'Prelude.IO' actions since it puts the last function first.
--
-- >>> print <! negate <! recip <! succ <! 3
-- -0.25
--
-- The difference between this and ('<|') is that this evaluates its argument
-- before passing it to the function.
--
-- >>> const True <| undefined
-- True
-- >>> const True <! undefined
-- *** Exception: Prelude.undefined
-- ...
--
-- Note that ('<!') and ('!>') have the same precedence, so they cannot be used
-- together.
--
-- >>> -- This doesn't work!
-- >>> -- print <! 3 !> succ !> recip !> negate
--
-- prop> \ x -> (f <! x) == seq x (f x)
--
-- prop> \ x -> (g <! f <! x) == let y = seq x (f x) in seq y (g y)
infixr 0 <!

(<!) :: (a -> b) -> a -> b
f <! x = apply' x f

-- | Strict function application. This function usually isn't necessary, but it
-- can be more readable than some alternatives when used with higher-order
-- functions like 'Prelude.map'.
--
-- >>> map (apply' 2) [succ, recip, negate]
-- [3.0,0.5,-2.0]
--
-- The different between this and 'apply' is that this evaluates its argument
-- before passing it to the function.
--
-- >>> apply undefined (const True)
-- True
-- >>> apply' undefined (const True)
-- *** Exception: Prelude.undefined
-- ...
--
-- In general you should prefer using an explicit lambda or operator section.
--
-- >>> map (\ f -> 2 !> f) [succ, recip, negate]
-- [3.0,0.5,-2.0]
-- >>> map (2 !>) [succ, recip, negate]
-- [3.0,0.5,-2.0]
-- >>> map (<! 2) [succ, recip, negate]
-- [3.0,0.5,-2.0]
--
-- prop> \ x -> apply' x f == seq x (f x)
apply' :: a -> (a -> b) -> b
apply' x f = seq x (apply x f)
