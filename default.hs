import Utils ((\.), (|>))
import Parser as p
import Control.Applicative ((<|>), (<*>), (*>), (<*))

main :: IO ()
main = putStrLn "Hello from $*:"
      >> getContents >>= putStr . unlines . take 5 . lines
-- main = getContents >>= putStrLn . show . solve . map format . lines

format :: String -> [Int]
format = P.mkInput
        \. P.runP ( undefined )
        \. P.value
        \. either (error . ("day??.format: bad input: "++)) id

solve :: [Int] -> Int
solve = undefined
