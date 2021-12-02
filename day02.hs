import Utils ((\.), (|$>))
import Utils (onSnd, onPair, dup)
import Parser as P
import Control.Applicative ((<|>))

type Horiz = Int
type Depth = Int
type Pos = (Horiz, Depth)

main :: IO ()
main = putStrLn "Hello from day02:"
      >> getContents
      >>= putStr . unlines . (:[]) . show . solve . map format . lines

format :: String -> Pos -> Pos
format = P.mkInput
        \. P.runP ((fdP <|> upP <|> dnP) <* P.optP (P.charP '\r') <* P.eofP)
        \. P.value
        \. either (error . ("day02.format: bad input: "++)) (onPair $ dup (+))
        \. onPair

fdP :: Parser Pos
fdP = (P.strP "forward " *> P.numP)
    |$> (flip (,) 0)

upP :: Parser Pos
upP = (P.strP "up " *> P.numP)
    |$> (,) 0
    |$> onSnd ((-1) *)

dnP :: Parser Pos
dnP = (P.strP "down " *> P.numP)
    |$> (,) 0

solve :: [Pos -> Pos] -> Int
solve = foldr ($) (0, 0)
        \. uncurry (*)
