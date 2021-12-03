import Utils ((\.), (|>), (|$>), onSnd, onPair, dup)
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
solve xss = let size = length xss in
    zipWith (-) (repeat size) (foldr (zipWith (+)) (repeat 0) xss)
    |> map (> (div size 2))
    |> dup
    |> onSnd (map $ not)
    |> onPair (dup fromBits)
    |> uncurry (*)

fromBit :: Bool -> Int
fromBit = bool 0 1

fromBits :: [Bool] -> Int
fromBits = foldl (\ acc b -> acc*2 + fromBit b) 0
