import Utils ((\.), (...), (\..), fork, hook, hook2, assert,
    onFst, onPair, onBoth, loop, bool)
import Parser as P
import Control.Applicative (some)
import Data.List (sort, sortBy, group, groupBy)

type Timer = Int
type Amount = Int

main :: IO ()
main = getContents >>= putStrLn . show . solve . head . map format . lines

format :: String -> [Int]
format = P.mkInput
        \. P.runP ( (:) <$> P.numP <*> some (P.charP ',' *> P.numP)
            <* P.optP (P.charP '\r') <* P.eofP )
        \. P.value
        \. either (error . ("day06.format: bad input: "++)) id

solve :: [Int] -> Int
solve = sort
    \. group
    \. fmap (fork (,) head length)
    \. (,) 256
    \. loop (uncurry oneDay)

oneDay :: Int -> [(Timer, Amount)] -> Either Int (Int, [(Timer, Amount)])
oneDay = hook (bool (
        const $
        fmap snd
        \. foldr (+) 0
        \. Left
    ) (
        hook2 (,) (
            assert (length \. (<=9))
            \. fmap (uncurry breed)
            \. concat
            \. sortBy (curry $ onBoth fst \. uncurry compare)
            \. groupBy (curry $ onBoth fst \. uncurry (==))
            \. fmap (foldr (onPair (const, (+)) \. onPair) (0, 0))
        )
        \.. (
            assert (snd \. length \. (<=9))
            \. onFst dec
            \. Right
        )
    )) (>0)

breed :: Timer -> Amount -> [(Timer, Amount)]
breed = hook (bool (
            (:[]) ... (dec \. (,))
        ) (
            const $
            fork (:) ((,) 6) ((,) 8 \. (:[]))
        )) (<=0)

dec :: Num a => a -> a
dec = flip (-) 1
