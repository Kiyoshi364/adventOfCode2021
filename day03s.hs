import Utils ((\.), (|>), (|$>), (...), hook, onPair, onBoth, dup, loop)
import Parser as P
import Control.Applicative
import Data.Char (digitToInt)
import Data.Bool (bool)

main :: IO ()
main = getContents >>= putStrLn . show . solve . map format . lines

format :: String -> [Int]
format = P.mkInput
        \. P.runP bitsP
        \. P.value
        \. either (error . ("day03.format: bad input: "++)) id

bitsP :: Parser [Int]
bitsP = some ((P.charP '0' <|> P.charP '1') |$> digitToInt)
    <* P.optP (P.charP '\r') <* P.eofP

solve :: [[Int]] -> Int
solve = (,) 0
        \. dup
        \. curry onPair findO2 findCO2
        \. onBoth (map (==1) \. fromBits)
        \. uncurry (*)

findO2 :: (Int, [[Int]]) -> [Int]
findO2 = loop (\ (b, xss) ->
            hook zip (repeat . mostCommonBit b) xss
            |> filter (\ (bit, xs) -> fromBit bit == (xs !! b))
            |> map snd
            |> hook
                (bool (head \. Left) ((,) (b+1) \. Right))
                (length \. (>1))
        )

findCO2 :: (Int, [[Int]]) -> [Int]
findCO2 = loop (\ (b, xss) ->
            hook zip (repeat . leastCommonBit b) xss
            |> filter (\ (bit, xs) -> fromBit bit == (xs !! b))
            |> map snd
            |> hook
                (bool (head \. Left) ((,) (b+1) \. Right))
                (length \. (>1))
        )

mostCommonBit :: Int -> [[Int]] -> Bool
mostCommonBit x = map (!!x) \. findMostCommon

findMostCommon :: [Int] -> Bool
findMostCommon xs = let (d, m) = divMod (length xs) 2 in
    foldr (+) (0) xs
    |> bool (> d) (>= d) (m == 0)

leastCommonBit :: Int -> [[Int]] -> Bool
leastCommonBit = not ... mostCommonBit

fromBit :: Bool -> Int
fromBit = bool 0 1

fromBits :: [Bool] -> Int
fromBits = foldl (\ acc b -> acc*2 + fromBit b) 0
