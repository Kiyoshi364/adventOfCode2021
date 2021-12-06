import Utils ((\.), (|>))
import Parser as P

main :: IO ()
main = getContents >>= putStrLn . show . solve . map format . lines

format :: String -> [Int]
format = P.mkInput
        \. P.runP ( undefined )
        \. P.value
        \. either (error . ("day??.format: bad input: "++)) id

solve :: [Int] -> Int
solve = undefined
