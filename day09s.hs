-- import Utils ((\.), (|>), (\..), fork, fork2, hook, hook2,
--     onFst, onSnd, onPair, onBoth, dup)
import Utils ((\.), onBoth)
import Parser as P
import Control.Applicative ((<|>), some)
import Data.Char (digitToInt)
import Data.List (sortBy, groupBy)

main :: IO ()
main = getContents >>= putStrLn . show . solve . map format . lines
    >> putStrLn "This is not finished (and may never be...)"

format :: String -> [Int]
format = P.mkInput
        \. P.runP ( some digitP )
        \. P.value
        \. either (error . ("day09.format: bad input: "++)) id

digitP :: Parser Int
digitP = digitToInt <$> ( P.charP '0' <|> P.charP '1' <|> P.charP '2' <|>
    P.charP '3' <|> P.charP '4' <|> P.charP '5' <|> P.charP '6' <|>
    P.charP '7' <|> P.charP '8' <|> P.charP '9')

solve :: [[Int]] -> Int
solve = mkBasins
    \. sortBy (flip compare)
    \. take 3
    \. foldr (*) 1

mkBasins :: [[Int]] -> [Int]
mkBasins = horizBasin
        -- \. fmap (foldr (
        --     undefined
        -- ) ((0,0),[]))
        -- \. hook (flip (,)) (undefined)
        -- \. uncurry (foldr (
        --     joinLines
        --     \. undefined
        -- ))
        \. const []

horizBasin :: [[Int]] -> [[[Int]]]
horizBasin = fmap (
        groupBy $ curry $ onBoth (/=9) \. uncurry (&&)
        \. undefined
    )

{- Not done
type Pos = (Int, Int)
type Basin = ([Pos], Int)

joinLines :: [[Basin]] -> [Int]
joinLines = foldr (joinLine \. undefined) []

joinLine :: [Basin] -> [Basin] -> ([Basin], [Int])
joinLine = rec Nothing ([], [])
  where
    rec :: Maybe Basin -> ([Basin], [Int]) -> [Basin] -> [Basin] -> ([Basin], [Int])
    rec Nothing          ret ubs                  []
        = finish ret |> onFst (++ubs)
    rec Nothing          ret     []               dbs
        = finish ret |> onSnd (flip (++) $ fmap (snd) dbs)
    rec Nothing          ret ubs@(u@(ups, _ ):us) dbs@((dps, dn):ds)
        = if match ups dps
        then rec (Just u) ret us dbs
        else if isMin ups dps
            then rec Nothing (onFst (u:)  ret)  us dbs
            else rec Nothing (onSnd (dn:) ret) ubs ds
    rec (Just (nps, nn)) ret ubs@((ups, un):us) dbs@((dps, dn):ds)
        = if match ups dps
        then rec (Just (nps++ups, nn+un)) ret us dbs
        else if isMin ups dps
            then error "Absurd: ups is smaller then dps, but there's a nps"
            else rec Nothing (onFst ((nps, nn+dn):) ret) ubs ds
    match :: [Pos] -> [Pos] -> Bool
    match ups dps = foldr (\ p -> foldr (lineInter p \. (||)) False ups |> (||)) False dps
    isMin :: [Pos] -> [Pos] -> Bool
    isMin ups dps = foldr (snd \. max) (-1) ups < foldr (fst \. min) 10 dps
    finish :: ([Basin], [Int]) -> ([Basin], [Int])
    finish = onPair (reverse, reverse)
-}

-- joinLine = hook2 (flip (foldr (
--         -- Basin -> ([Basin], [[Int]]) -> ([Basin], [[Int]])
--         undefined :: Basin -> ([Basin], [[Int]]) -> ([Basin], [[Int]])
--     ))) (flip (,) [])

{- This works
lineInter :: Pos -> Pos -> Bool
lineInter = fork2 (||) (
        fork2 (onPair \.. uncurry (||))
            (const \.. onBoth isBetween) (const dup)
    ) (
        fork2 (onPair \.. uncurry (||))
            (flip const \.. onBoth isBetween) (flip const \.. dup)
    )

isBetween :: Int -> (Int, Int) -> Bool
isBetween = (dup \. onPair ((>=), (<=)) \. onPair) \.. uncurry (&&)
-}
