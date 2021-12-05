import Utils ((\.), (|>), fork, onFst, onSnd, onPair, onBoth, mapif)
import Parser as P

main :: IO ()
main = getContents >>= putStrLn . show . solve . map format . lines

type Point = (Int, Int)
type Line = (Point, Point)
type Map = [[(Point, Int)]]

format :: String -> Line
format = P.mkInput
        \. P.runP ( lineP <* P.optP (P.charP '\r') <* P.eofP )
        \. P.value
        \. either (error . ("day05.format: bad input: "++)) id

lineP :: Parser Line
lineP = (,) <$> pointP <* P.strP " -> " <*> pointP

pointP :: Parser Point
pointP = (,) <$> P.numP <* P.charP ',' <*> P.numP

emptyMap :: Map
emptyMap = foldr (buildCoords) [] [0..]
        |> fmap (fmap (flip (,) 0))
 where
  -- (\ y ls -> zip [0..] (repeat y) : ls )
  buildCoords = (:) . zip [0..] . repeat

solve :: [Line] -> Int
solve = fork (\ (maxx, maxy) -> take maxx \. fmap (take maxy))
        (foldr (maxCoord \. curry maxCoord) (0,0) \. onBoth (+1))
        (foldr (drawLine) emptyMap)
    \. concat
    \. filter (snd \. (>1))
    \. length

maxCoord :: Line -> Point
maxCoord = onFst (onBoth max) \. uncurry onPair

drawLine :: Line -> Map -> Map
drawLine line = fmap $ mapif (fst \. inLine line) (onSnd (+1))

inLine :: Line -> Point -> Bool
inLine l@((x1,y1), (x2,y2)) p@(x, y)
    | ax == 0           = xx == 0 && btn 0 yy by
    | by == 0           = yy == 0 && btn 0 xx ax
    | abs ax == abs by  = abs xx == abs yy && btn 0 xx ax && btn 0 yy by
    | otherwise         = error $ "aaaaa: " ++ show l ++ ", " ++ show p
  where
    xx = x - x1
    ax = x2 - x1
    yy = y - y1
    by = y2 - y1
    btn a b c = a <= b && b <= c || c <= b && b <= a
