import Utils

main :: IO ()
main = putStrLn "Hello from day01:"
      >>  getContents
      >>= putStr . unlines . (:[]) . show . solve . map read . lines

solve :: [Int] -> Int
solve = hook (zipWith (-)) (tail ^: 3)
        \. filter (> 0)
        \. length
