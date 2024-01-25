# Haskell abstract syntax using GADTs

This example uses the native Haskell PGF library, but as opposed to the simplest example, we use the flag `--haskell=gadt`.

The file `MiniLang.hs` in this directory has been generated automatically with the following command.

```bash
gf -make -f haskell --haskell=gadt --haskell=lexical --lexical=A,Adv,Conj,Det,N,PN,Pron,Prep,V,V2 ../resource/MiniLangEng.gf
```

The differences between the files are pretty significant. Instead of mutually recursive datatypes from [../MiniLang.hs](../MiniLang.hs) and [../advanced-pgf2/MiniLang.hs](../advanced-pgf2/MiniLang.hs), now the whole `Tree` is a single GADT, parameterized by a dummy type that corresponds to the original GF category.

The crucial difference is that now we only need a single function to transform the whole tree.

```haskell
-- Transform a subtree, keep rest of the tree intact
toReflexive :: forall a . Tree a -> Tree a
toReflexive tree = case tree of
  -- If argument tree matches, do the transformation
  GPredVP subj (GComplV2 v2 obj) ->
    if isSame subj obj
      then GPredVP subj (GReflV2 v2)
      else tree

  -- If argument tree doesn't match, apply toReflexive to all subtrees
  _ -> composOp toReflexive tree
```

The type signature of this function `Tree a -> Tree a`, so we can apply it recursively with `composOp` to all its subtrees. Those subtrees are also of type `Tree a`, but possibly a different `a` than the parent tree's.

All that matters is that the transformation step returns the same `Tree a`. In this case, we match the subtree `GPredVP subj (GComplV2 v2 obj)` and return `GPredVP subj (GReflV2 v2)`—clearly these two subtrees are of the same type, because they use the same constructor.