module Main where

import PGF
import System.Environment (getArgs)

main :: IO ()
main = do
  file:_ <- getArgs
  gr     <- readPGF file
  putStrLn "Write your sentence here to translate it."
  interact' (translate gr)

translate :: PGF -> String -> String
translate gr s = case parseAllLang gr (startCat gr) s of
  (lg,t:_):_ -> unlines [linearize gr l t | l <- languages gr, l /= lg]
  _ -> "NO PARSE"

-- The library function interact,
-- written here to make the code more understandable.
interact' :: (String -> String) -> IO ()
interact' f = do s <- getLine
                 putStrLn (f s)
