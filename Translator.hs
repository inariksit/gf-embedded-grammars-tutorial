import PGF

main :: IO ()
main = do
  gr <- readPGF "MiniLang.pgf" -- Open the PGF file
  putStrLn "Write your sentence here to translate it."
  s <- getLine -- Get the sentence from the user during the program's run
  putStrLn (translate gr s)
  putStrLn "Thanks for using the great GF translator!"

-- A pure function that parses the given string with the given PGF
-- and translates it into all other languages in the PGF.
translate :: PGF -> String -> String
translate gr s = case parseAllLang gr (startCat gr) s of
  (lg,t:_):_
    -> unlines [ linearize gr l t
               | l <- languages gr
               , l /= lg ]
  _ -> "NO PARSE"
