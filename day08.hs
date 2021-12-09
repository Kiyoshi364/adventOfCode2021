import Utils ((\.), (|>))
import Parser as P
import Control.Applicative ((<|>), some)

main :: IO ()
main = getContents >>= putStrLn . show . solve . map format . lines

format :: String -> ([String], [String])
format = P.mkInput
        \. P.runP ( (,) <$> some digitsP <* P.strP "| "
            <*> some digitsP <* P.charP '\r' <* P.eofP)
        \. P.value
        \. either (error . ("day08.format: bad input: "++)) id

digitsP :: Parser String
digitsP = some digitP <* P.optP (P.charP ' ')

digitP :: Parser Char
digitP = fmap P.charP "abcdefg" |> foldr1 (<|>)

solve :: [([String], [String])] -> Int
solve = fmap ( snd \. filter isSimpleDigit )
        \. concat
        \. length

isSimpleDigit :: String -> Bool
isSimpleDigit = length \. (==) \. flip fmap [2, 3, 4, 7] \. foldr (||) False
