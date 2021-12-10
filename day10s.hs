import Utils ((\.), fork, hook, hook2, onSnd, onPair, bool)
import Parser as P
import Control.Applicative ((<|>), some, many)
import Data.List (sort)

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

format :: String -> [Chunk]
format = P.mkInput
        \. P.runP (some chunkP <* P.optP (P.charP '\r') <* P.eofP )
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

solve :: [[Chunk]] -> Int
solve = fmap last
        \. filter (corrupted \. not)
        \. fmap (
            complete
            \. fmap cToS
            \. foldl ((*5) \. (+)) 0
        )
        \. median

corrupted :: Chunk -> Bool
corrupted = points \. snd \. (/='?')

complete :: Chunk -> String
complete = hook (
            flip $
            hook (
                bool (const $ const "") (op \. toEnd \. (:[]) \. flip (++))
            ) (ed \. (=='?'))
        ) (
            ck \.
            hook (
                bool (last \. complete) (const "")
            ) (length \. (==0))
        )

-- From day07, but fixed :)
median :: [Int] -> Int
median = sort
        \. hook (,) length
        \. hook (drop \. onSnd) (fst \. flip div 2 \. (+) (-1))
        \. fork (
            bool (take 2 \. avg) (tail \. head)
        ) (fst \. flip rem 2 \. (/=0)) snd
  where
    avg :: Integral a => [a] -> a
    avg = foldr (+) 0 \. flip div 2

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

toEnd :: Char -> Char
toEnd a = case a of
    '(' -> ')'
    '[' -> ']'
    '{' -> '}'
    '<' -> '>'
    _   -> error $ "toEnd: Invalid opening char: " ++ show a

cToS :: Char -> Int
cToS c = case c of
    ')' -> 1
    ']' -> 2
    '}' -> 3
    '>' -> 4
    _   -> error $ "cToS: Invalid closing char: " ++ show c
