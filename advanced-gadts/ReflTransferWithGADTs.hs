{-# LANGUAGE GADTs, RankNTypes #-}
-- This program transforms a sentence of the form
-- I like me ; you see you ; John asks John
-- into the reflexive versions:
-- I like myself ; you see yourself ; John asks himself
-- The subject and the object must be the same abstract syntax function.

import PGF hiding (Tree)
import MiniLang  -- generated automatically from ../resource/MiniLangEng.gf

transfer :: Expr -> Expr
transfer t = gf $ toReflexive $ (fg t :: GUtt)

-- Example usage of composOp: Transform a subtree, keep rest of the tree intact
toReflexive :: forall a . Tree a -> Tree a
toReflexive tree = case tree of
  -- If argument tree matches, do the transformation
  GPredVP subj (GComplV2 v2 obj) ->
    if isSame subj obj
      then GPredVP subj (GReflV2 v2)
      else tree

  -- If argument tree doesn't match, apply toReflexive to all subtrees
  _ -> composOp toReflexive tree

-- NB. this is not a valid solution for all possible trees,
-- but getLex demonstrates the usage of composOpMonoid.
isSame :: GNP -> GNP -> Bool
isSame tree1 tree2 = and $ zipWith (==) (getLex tree1) (getLex tree2)

-- Example usage of composOpMonoid: if argument matches, return some result in a monoid
-- If argument doesn't match, apply getLex to all subtrees
getLex :: forall a . Tree a -> [String]
getLex tree = case tree of
  LexA    s -> [s]
  LexDet  s -> [s]
  LexN    s -> [s]
  LexPN   s -> [s]
  LexPrep s -> [s]
  LexPron s -> [s]
  LexV    s -> [s]
  LexV2   s -> [s]
  x -> composOpMonoid getLex x

main :: IO ()
main = do
  gr <- readPGF "../MiniLang.pgf"
  putStrLn "Write your sentence here (e.g. 'I see me', 'John sees John'), I will transform it into reflexive, if it has the same subject and object"
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
