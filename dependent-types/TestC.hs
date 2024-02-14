import PGF2
import Data.Map ((!))
import Data.Either (rights)

main :: IO ()
main = do
    gr <- readPGF "Nat.pgf"
    let conc = languages gr ! "NatC"
        span = startCat gr
    let depth5 = 9.010914

    let parseString str = do
            let ParseOk res = parse conc span str
                resUpto5 = toDepth depth5 res
                len = length resUpto5
            putStr $ "Parsed the string \"" <> str <> "\", results: "
            print len
            if len < 12
                then mapM_ (putStrLn . showExpr [] . fst) resUpto5
                else pure ()



    let generated = toDepth depth5 $ generateAll gr span
    --mapM_ print generated
    putStr "Generated trees of depth 5: "
    print $ length generated

    -- Parsing is not very useful
    -- Removing variants from the grammar doesn't help
    parseString "0 — 1" -- 33 results
    parseString "1 — 0" -- 33 results
    parseString "0 — 0 +1" -- 11 results, 8 of which didn't parse the first argument
    parseString "0 +1 +1 +1 +1 — 0 +1 +1 +1 +1 +1" -- only the correct result


toDepth :: Float -> [(Expr, Float)] -> [(Expr, Float)]
toDepth depth = takeWhile (\(_,p) -> p <= depth)
