import Utils ((\.), (|>), fork, hook, onFst, onSnd, onPair, mapif, bool)
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
solve = hook wrapper (onPair (head, head))
        \. onSnd preScore
        \. uncurry (*)

wrapper :: (Int, Board) -> ([Int], [Board]) -> (Int, Board)
wrapper a x = case uncurry winner x of
    Nothing            -> a
    Just (Right  x'  ) -> wrapper a x'
    Just (Left (s, b)) -> x
        |> onFst (drop 1)
        |> onSnd (fmap (mark s) \. filter (boardDone \. not))
        |> wrapper (s, b)

winner :: [Int] -> [Board] -> Maybe (Either (Int, Board) ([Int], [Board]))
winner  []    = const Nothing
winner (s:ss) = fmap (mark s)
        \. hook
            (maybe (Right) (Left \. const))
            findBoardDone
        \. either ((,) s \. Left) ((,) ss \. Right)
        \. Just

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
