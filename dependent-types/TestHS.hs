import PGF

instance Show ParseOutput where
    show = showPO

showPO :: ParseOutput -> String
showPO (ParseFailed i) = "Parse failed at token " <> show i
showPO (TypeError _) = "The parsing was successful but none of the trees is type correct."
showPO (ParseOk ts) = "ParseOk"
showPO ParseIncomplete = "ParseIncomplete"

main :: IO ()
main = do
    gr <- readPGF "Nat.pgf"
    let Just lang = readLanguage "NatC"
        span = startCat gr
        spans = generateAllDepth gr span (Just 5)
    putStrLn "Generated all spans up to depth 5"
    mapM_ (putStrLn . showExpr []) spans
    mapM_ (putStrLn . linearize gr lang) spans
    print $ length spans
    putStrLn "------"

    let fromExpr = mkApp (mkCId "FromTo") (mkMeta <$> [1..3])
        spans_explicit_FromTo = generateFromDepth gr fromExpr (Just 5)
    mapM_ (putStrLn . showExpr []) spans_explicit_FromTo
    print $ length spans_explicit_FromTo
    mapM_ (putStrLn . linearize gr lang) spans_explicit_FromTo

    tryParsing gr lang span "0 — 0 +1 +1 +1 +1 +1"
    tryParsing gr lang span "0 +1 — 0 +1 +1 +1 +1 +1"
    tryParsing gr lang span "0 +1 +1  — 0 +1 +1 +1 +1 +1"
    tryParsing gr lang span "0 +1 +1 +1 — 0 +1 +1 +1 +1 +1"
    tryParsing gr lang span "0 +1 +1 +1 +1 — 0 +1 +1 +1 +1 +1"

    tryParsing gr lang span "0 — 5"
    tryParsing gr lang span "1 — 5"
    tryParsing gr lang span "2 — 5"
    tryParsing gr lang span "3 — 5"
    tryParsing gr lang span "4 — 5"

tryParsing :: PGF -> Language -> Type -> String -> IO ()
tryParsing pgf lang typ str = do
    let trees = parse pgf lang typ str
    case trees of
      _:_ -> do putStrLn $ "Successfully parsed " <> str
                mapM_ (putStrLn . showExpr []) trees
                mapM_ (putStrLn . linearize pgf lang) trees
      [] -> do putStrLn $ "Failed parsing " <> str
               let (output, _bstr) = parse_ pgf lang typ Nothing str
               putStrLn $ " * " <> show output
