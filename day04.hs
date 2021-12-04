import Utils ((\.), fork, hook, onFst, onSnd, loop, mapif, bool)
import Parser as P
import Control.Applicative (some)

type Board = [[(Bool, Int)]]

main :: IO ()
main = getContents >>= putStrLn . show . solve . format

format :: String -> ([Int], [Board])
format = P.mkInput
        \. P.runP ( (,) <$> stonesP <*> tablesP <* P.eofP )
        \. P.value
        \. either (error . ("day04.format: bad input: "++)) id

stonesP :: Parser [Int]
stonesP = (++) <$> some (P.numP <* P.charP ',')
    <*> fmap (:[]) P.numP
    <* P.lfP <* P.lfP

tablesP :: Parser [Board]
tablesP = some $ tableP <* P.optP P.lfP

tableP :: Parser Board
tableP = fmap (fmap ((,) False)) <$> countP 5 lineP
  where
    lineP :: Parser [Int]
    lineP = (:) <$> (P.optP P.wsP *> P.numP)
        <*> countP 4 (P.wsP *> P.numP)
        <* P.optP P.lfP

solve :: ([Int], [Board]) -> Int
solve = loop (uncurry winner)
        \. onSnd preScore
        \. uncurry (*)

winner :: [Int] -> [Board] -> Either (Int, Board) ([Int], [Board])
winner  []    = error "Ran out of stones"
winner (s:ss) = fmap (mark s)
        \. hook
            (maybe (Right) (Left \. const))
            findBoardDone
        \. either ((,) s \. Left) ((,) ss \. Right)

mark :: Int -> Board -> Board
mark s = fmap $ mapif (snd \. (==s)) (onFst $ const True)

findBoardDone :: [Board] -> Maybe Board
findBoardDone = filter boardDone
            \. hook
                (bool (const Nothing) (head \. Just))
                (length \. (>=1))

boardDone :: Board -> Bool
boardDone = fmap (fmap fst)
        \. fork (||) rows cols
  where
    rows :: [[Bool]] -> Bool
    rows = foldr (foldr (&&) True \. (||)) False
        -- fmap and \. or
    cols :: [[Bool]] -> Bool
    cols = foldr (zipWith (&&)) (repeat True)
        \. foldr (||) False

preScore :: Board -> Int
preScore = concat
        \. filter (fst \. not)
        \. fmap snd
        \. foldr (+) 0
