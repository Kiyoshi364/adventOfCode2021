import Utils

main :: IO ()
main = getContents >>= putStrLn . show . solve . map read . lines

solve :: [Int] -> Int
solve = hook (zipWith (-)) tail
        \. filter (> 0)
        \. length
