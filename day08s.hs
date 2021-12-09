import Utils ((\.), (|>), (\..), fork, fork2, onPair)
import Parser as P
import Control.Applicative ((<|>), some)
import Data.List (sortBy, find)

type Digit = String
type Info = (Digit, Digit)

main :: IO ()
main = getContents >>= putStrLn . show . solve . map format . lines

format :: String -> ([Digit], [Digit])
format = P.mkInput
        \. P.runP ( (,) <$> digitsP <* P.strP "| "
            <*> digitsP <* P.charP '\r' <* P.eofP)
        \. P.value
        \. either (error . ("day08.format: bad input: "++)) id

digitsP :: Parser [Digit]
digitsP = some ( digitP <* P.optP (P.charP ' ') )

digitP :: Parser Digit
digitP = some validCharP
  where
    validCharP = fmap P.charP "abcdefg" |> foldr1 (<|>)

solve :: [([Digit], [Digit])] -> Int
solve = fmap (
            fork (,) (uncurry (++) \. trainDecoder) snd
            \. uncurry fmap
            \. foldl ((*) 10 \. (+)) 0
        )
        \. foldl (+) 0

trainDecoder :: [Digit] -> Digit -> Int
trainDecoder = sortByLen
        \. getOneFour
        \. decode

getOneFour :: [Digit] -> Info
getOneFour = fork (,) (find $ length \. (==2)) (find $ length \. (==4))
        \. onPair (maybe (error "one not found!" ) id,
                   maybe (error "four not found!") id)

decode :: Info -> Digit -> Int
decode i@(one, four) d = case length d of
    2 -> 1
    3 -> 7
    4 -> 4
    5 -> if inter one d == one
        then 3
        else case length (inter four d) of
            2 -> 2
            3 -> 5
            _ -> error $ "unknown digit: " ++ info
    6 -> if length (inter one d) == 1
        then 6
        else if length (inter four d) == 3
            then 0
            else 9
    7 -> 8
    _ -> error $ "digit too large: " ++ info
  where
    info = show i ++ " " ++ show d

sortByLen :: [[a]] -> [[a]]
sortByLen = sortBy (fork2 compare (const \.. length) (flip const \.. length))

inter :: Eq a => [a] -> [a] -> [a]
inter = fork2 filter (flip const \.. flip elem) const

{-
1: len = 2
7: len = 3
4: len = 4

3: len = 5 & inter 1 = 1
2: len = 5 & len (2 inter 4) = 1
5: len = 5 & len (5 inter 4) = 2

0: len = 6 & len (0 inter 4) = 3
6: len = 6 & len (6 inter 1) = 1
9: len = 6 & len (9 inter 4) = 4

8: len = 7
-}
