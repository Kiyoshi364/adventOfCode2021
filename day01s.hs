import Utils

main :: IO ()
main = putStrLn "Hello from day01:"
      >>  getContents
      >>= putStr . unlines . (:[]) . show . solve . map read . lines

solve :: [Int] -> Int
solve xs = zipWith3 ((+) ... (+)) (tail $ tail xs) (tail xs) xs
        |> hook (zipWith (-)) tail
        |> filter (> 0)
        |> length
