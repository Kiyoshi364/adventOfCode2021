module Utils
    ( (\.)
    , (|>)
    , (...)
    , fork, hook
    ) where

(\.) :: (a -> b) -> (b -> c) -> a -> c
(\.) = flip (.)
infixr 9 \.

(|>) :: a -> (a -> b) -> b
(|>) = flip ($)
infixl 0 |>

(...) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(...) = (.) (.) (.)

fork :: (a1 -> a2 -> b) -> (a -> a1) -> (a -> a2) -> a -> b
fork h f g x = h (f x) (g x)

hook :: (a1 -> a -> b) -> (a -> a1) -> a -> b
hook h f = fork h f id
