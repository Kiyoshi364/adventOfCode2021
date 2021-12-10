import Utils ((\.), (\..), fork, fork2, hook, onSnd, onPair, bool)
import Parser as P
import Control.Applicative ((<|>), some)
import Data.Char (digitToInt)

main :: IO ()
main = getContents >>= putStrLn . show . solve . map format . lines

format :: String -> [Int]
format = P.mkInput
        \. P.runP ( some digitP )
        \. P.value
        \. either (error . ("day09.format: bad input: "++)) id

digitP :: Parser Int
digitP = digitToInt <$> ( P.charP '0' <|> P.charP '1' <|> P.charP '2' <|>
    P.charP '3' <|> P.charP '4' <|> P.charP '5' <|> P.charP '6' <|>
    P.charP '7' <|> P.charP '8' <|> P.charP '9')

solve :: [[Int]] -> Int
solve = mkAdjacents
    \. filter (onSnd (foldr min 10) \. uncurry (<))
    \. fmap (fst \. (+1))
    \. foldr (+) 0

mkAdjacents :: [[Int]] -> [(Int, [Int])]
mkAdjacents = fork (
            zipWith (onPair (const, (++)) \. onPair)
        ) (horizAdj \. concat)
        (transpose \. horizAdj \. transpose \. concat)

horizAdj :: [[Int]] -> [[(Int, [Int])]]
horizAdj = addPadding
    \. fmap (
        window 3
        \. fmap (fork (,) (!!1) (
            fork (++) (take 1) (drop 2)
        ))
    )

addPadding :: [[Int]] -> [[Int]]
addPadding = zipWith (:) (repeat 10)
        \. fmap (foldr (:) [10])

window :: Int -> [a] -> [[a]]
window = fork2 (:) take (
            fork2 (
                (,) \..
                hook (
                    bool (const []) (uncurry window)
                ) (onSnd length \. uncurry (<=))
            ) (const) (const $ drop 1)
        )

transpose :: [[a]] -> [[a]]
transpose = foldr (zipWith (:)) (repeat [])
