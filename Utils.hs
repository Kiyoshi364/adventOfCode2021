module Utils
    ( (\.) , (|>) , (|$>) , (...) , (^:)
    , fork, hook
    , onFst, onSnd, onPair, onBoth, dup
    , loop
    ) where

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

(^:) :: (a -> a) -> Int -> a -> a
(^:) f n x
    | n <  0    = error $ "^: received negative number (" ++ show n ++ ")"
    | n == 0    = x
    | otherwise = f ^: (n-1) $ f x

fork :: (a1 -> a2 -> b) -> (a -> a1) -> (a -> a2) -> a -> b
fork h f g x = h (f x) (g x)

hook :: (a1 -> a -> b) -> (a -> a1) -> a -> b
hook h f = fork h f id

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
