-- This program transforms a sentence of the form
-- I like me ; you see you ; John asks John
-- into the reflexive versions:
-- I like myself ; you see yourself ; John asks himself
-- The subject and the object must be the same abstract syntax function.

  import PGF
  import MiniLang  -- generated automatically from resource/MiniLangEng.gf

  transfer :: Expr -> Expr
  transfer = gf . toReflexive . fg

  -- Just a wrapper for the more interesting trasfer functions
  -- Need this because Utt is the start category; the strings we input in the translator loop are parsed as Utt by default
  toReflexive :: GUtt -> GUtt
  toReflexive (GUttNP x) = GUttNP x -- NPs cannot be made reflexive
  toReflexive (GUttS s) = GUttS (toReflexiveS s)

  -- Another layer of wrapper
  toReflexiveS :: GS -> GS
  toReflexiveS s = case s of
    GCoordS conj s1 s2 -> GCoordS conj (toReflexiveS s1) (toReflexiveS s2)
    GUsePresCl pol cl -> GUsePresCl pol (toReflexiveCl cl)

  -- The relevant transfer function is on a Cl -> Cl level
  toReflexiveCl :: GCl -> GCl
  toReflexiveCl cl@(GPredVP subj vp) = -- PredVP is the only constructor for Cl in the mini resource
    case vp of
      GComplV2 v2 obj
        -> if show subj == show obj -- GNP has no Eq instance by default
            then GPredVP subj (GReflV2 v2)
            else cl
      _ -> cl -- Any other way to form VP: keep it unchanged

  main :: IO ()
  main = do
    gr <- readPGF "MiniLang.pgf"
    putStrLn "Write your sentence here, I will transform it into reflexive, if it has the same subject and object"
    putStrLn "Write quit to exit."
    loop (translate transfer gr)

  loop :: (String -> String) -> IO ()
  loop trans = do
    s <- getLine
    if s == "quit" then putStrLn "bye" else do
      putStrLn $ trans s
      loop trans

  translate :: (Expr -> Expr) -> PGF -> String -> String
  translate tr gr s = case parseAllLang gr (startCat gr) s of
    (lg,t:_):_ -> linearize gr lg (tr t)
    _ -> "NO PARSE"
