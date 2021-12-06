module Utils
    ( (\.) , (|>) , (|$>) , (...) , (\..) , (^:)
    , fork, fork2, hook, hook2
    , assert, assertWith
    , onFst, onSnd, onPair, onBoth, dup
    , loop, loopLast
    , ifelse, mapif, bool
    ) where

import Data.Bool (bool)

-- Combinators

(\.) :: (a -> b) -> (b -> c) -> a -> c
(\.) = flip (.)
infixr 9 \.

(|>) :: a -> (a -> b) -> b
(|>) = flip ($)
infixl 0 |>

(|$>) :: Functor f => f a -> (a -> b) -> f b
(|$>) = flip (<$>)
infixr 4 |$>

(...) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(...) = (.) (.) (.)

(\..) :: (a -> b -> c) -> (c -> d) -> a -> b -> d
(\..) = flip (...)

(^:) :: (a -> a) -> Int -> a -> a
(^:) f n x
    | n <  0    = error $ "^: received negative number (" ++ show n ++ ")"
    | n == 0    = x
    | otherwise = f ^: (n-1) $ f x

fork :: (a1 -> a2 -> b) -> (a -> a1) -> (a -> a2) -> a -> b
fork h f g x = h (f x) (g x)

fork2 :: (b1 -> b2 -> c) -> (a1 -> a2 -> b1) -> (a1 -> a2 -> b2)
         -> a1 -> a2 -> c
fork2 h f g x y = h (f x y) (g x y)

hook :: (a1 -> a -> b) -> (a -> a1) -> a -> b
hook h f = fork h f id

hook2 :: (a -> b1 -> c) -> (b -> b1) -> a -> b -> c
hook2 h f = fork2 h const (const f)

-- Assertions

assert :: Show a => (a -> Bool) -> a -> a
assert = assertWith $ ("assertion fail: "++) . show

assertWith :: (a -> String) -> (a -> Bool) -> a -> a
assertWith f p x
    | p x       =  x
    | otherwise = error $ f x

-- Pair

onFst :: (a -> c) -> (a, b) -> (c, b)
onFst f (a, b) = (f a, b)

onSnd :: (b -> c) -> (a, b) -> (a, c)
onSnd f (a, b) = (a, f b)

onPair :: (a -> a', b -> b') -> (a, b) -> (a', b')
onPair (fa, fb) = onFst fa . onSnd fb

onBoth :: (a -> a') -> (a, a) -> (a', a')
onBoth = onPair . dup

dup :: a -> (a, a)
dup = hook (,) id

-- Morphisms

loop :: (a -> Either b a) -> a -> b
loop f x = case f x of
    Right x' -> loop f x'
    Left  y  -> y

loopLast :: (a -> Either b a) -> a -> a
loopLast f x = case f x of
    Right x' -> loopLast f x'
    Left  _  -> x

-- Control

ifelse :: (a -> Bool) -> (a -> b) -> (a -> b) -> a -> b
ifelse p true false = hook (bool false true) p

mapif :: Functor f => (a -> Bool) -> (a -> a) -> f a -> f a
mapif p f = fmap $ ifelse p f id
