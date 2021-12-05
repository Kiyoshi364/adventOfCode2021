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
solve = filter horivert
    \. fork (\ (maxx, maxy) -> take maxx \. fmap (take maxy))
        (foldr (maxCoord \. curry maxCoord) (0,0) \. onBoth (+1))
        (foldr (drawLine) emptyMap)
    \. concat
    \. filter (snd \. (>1))
    \. length

horivert :: Line -> Bool
horivert = fork (||) (onBoth fst \. uncurry (==)) (onBoth snd \. uncurry (==))

maxCoord :: Line -> Point
maxCoord = onFst (onBoth max) \. uncurry onPair

drawLine :: Line -> Map -> Map
drawLine line = fmap $ mapif (fst \. inLine line) (onSnd (+1))

inLine :: Line -> Point -> Bool
inLine ((x1,y1), (x2,y2)) (x, y) = a1 <= x && x <= a2 && b1 <= y && y <= b2
  where
    a1 = min x1 x2
    a2 = max x1 x2
    b1 = min y1 y2
    b2 = max y1 y2
