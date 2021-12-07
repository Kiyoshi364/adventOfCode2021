import Utils ((\.), (\..), fork, fork2)
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

solve :: [Int] -> Int
solve = sort
      \. fork zip (foldr max 0 \. flip take [1..]) repeat
      \. fmap (uncurry costList)
      \. foldr1 min

costList :: Int -> [Int] -> Int
costList = ( absminus \.. (cost \. (+)) ) \. flip foldr 0

cost :: Int -> Int
cost = flip take [1..] \. foldr (+) 0

absminus :: Int -> Int -> Int
absminus = fork2 (-) max min
