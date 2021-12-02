import Utils ((\.), (|$>))
import Parser as P
import Control.Applicative ((<|>))

type Horiz = Int
type Depth = Int
type Aim   = Int
type Pos = (Horiz, Depth, Aim)

main :: IO ()
main = putStrLn "Hello from day02:"
      >> getContents
      >>= putStr . unlines . (:[]) . show . solve . map format . lines

format :: String -> Pos -> Pos
format = P.mkInput
        \. P.runP ((fdP <|> upP <|> dnP) <* P.optP (P.charP '\r') <* P.eofP)
        \. P.value
        \. either (error . ("day02.format: bad input: "++)) id

fdP :: Parser (Pos -> Pos)
fdP = (P.strP "forward " *> P.numP)
    |$> (\ x (h, d, a) -> (h+x, d+(a*x), a))

upP :: Parser (Pos -> Pos)
upP = (P.strP "up " *> P.numP)
    |$> (\ x (h, d, a) -> (h, d, a-x))

dnP :: Parser (Pos -> Pos)
dnP = (P.strP "down " *> P.numP)
    |$> (\ x (h, d, a) -> (h, d, a+x))

solve :: [Pos -> Pos] -> Int
solve = foldl (flip ($)) (0, 0, 0)
        \. (\ (h, d, _) -> (h, d))
        \. uncurry (*)
