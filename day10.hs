import Utils ((\.), fork, hook, hook2, onPair, bool)
import Parser as P
import Control.Applicative ((<|>), many)

data Chunk = Chunk
    { op :: Char
    , ck :: [Chunk]
    , med :: Maybe Char
    }

instance Show Chunk where
    show cnk@(Chunk o c _) = "/ " ++ o:[] ++ show c ++ e:[] ++ " \\"
      where e = ed cnk

ed :: Chunk -> Char
ed = med \. maybe '?' id

main :: IO ()
main = getContents >>= putStrLn . show . solve . map format . lines

format :: String -> Chunk
format = P.mkInput
        \. P.runP (chunkP <* P.optP (P.charP '\r') <* P.eofP )
        \. P.value
        \. either (error . ("day10.format: bad input: "++)) id

chunkP :: Parser Chunk
chunkP = Chunk <$> opP <*> many chunkP <*> edP

opP :: Parser Char
opP = P.charP '(' <|> P.charP '[' <|> P.charP '{' <|> P.charP '<'

edP :: Parser (Maybe Char)
edP = fmap Just
    (P.charP ')' <|> P.charP ']' <|> P.charP '}' <|> P.charP '>')
    <|> pure Nothing

solve :: [Chunk] -> Int
solve = fmap (
            points
            \. snd
            \. cToP
        )
        \. foldr (+) 0

points :: Chunk -> (Int, Char)
points = hook (
            hook2 errOp (
                hook (
                    bool (ed \. (,) 1) (const (1, '0'))
                ) (fork paren op ed)
            )
        ) (ck \. firstErr)

firstErr :: [Chunk] -> (Int, Char)
firstErr = fmap points \. foldl errOp (1, '0')

errOp :: (Int, Char) -> (Int, Char) -> (Int, Char)
errOp = flip $ hook2 (
            flip $ uncurry $
            bool const (onPair ((+), flip const) \. onPair)
        ) (hook (,) (snd \. (=='0')))

paren :: Char -> Char -> Bool
paren a b = case (a, b) of
    ('(', ')') -> True
    ('[', ']') -> True
    ('{', '}') -> True
    ('<', '>') -> True
    _          -> False

cToP :: Char -> Int
cToP c = case c of
    '0' -> 0
    ')' -> 3
    ']' -> 57
    '}' -> 1197
    '>' -> 25137
    '?' -> 0
    _   -> error $ "cToP: Invalid closing char: " ++ show c
