import Utils ((\.), (\..), fork, fork2, hook, onSnd, bool)
import Parser as P
import Control.Applicative (some)
import Data.List (sort)

main :: IO ()
main = getContents >>= putStrLn . show . solve . head . map format . lines

format :: String -> [Int]
format = P.mkInput
        \. P.runP ( (:) <$> P.numP <*> some (P.charP ',' *> P.numP)
            <* P.optP (P.charP '\r') <* P.eofP )
        \. P.value
        \. either (error . ("day07.format: bad input: "++)) id

solve :: [Int] -> (Int, Int)
solve = fork (,) solveOriginal solveByEricTrotta

solveOriginal :: [Int] -> Int
solveOriginal = sort
      \. fork zip (foldr max 0 \. flip take [1..]) repeat
      \. fmap (uncurry costList)
      \. foldr1 min

solveByEricTrotta :: [Int] -> Int
solveByEricTrotta = hook costList median

median :: [Int] -> Int
median = sort
        \. hook (,) length
        \. hook (drop \. onSnd) (fst \. flip div 2)
        \. fork (
            bool (take 2 \. avg) head
        ) (fst \. flip rem 2 \. (==0)) snd
  where
    avg :: Integral a => [a] -> a
    avg = foldr (+) 0 \. div 2

costList :: Int -> [Int] -> Int
costList = ( absminus \.. (+) ) \. flip foldr 0

absminus :: Int -> Int -> Int
absminus = fork2 (-) max min
