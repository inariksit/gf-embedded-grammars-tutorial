# Haskell bindings to the C runtime, aka. "PGF2"

The file `MiniLang.hs` in this directory has been generated automatically with the following command.

```bash
gf -make -f haskell --haskell=pgf2 --haskell=lexical --lexical=A,Adv,Conj,Det,N,PN,Pron,Prep,V,V2 ../resource/MiniLangEng.gf
```

## Differences to ../MiniLang.hs

The generated files are almost identical. Run the following command:
```bash
diff ../MiniLang.hs MiniLang.hs
```

The diff is very small:

```haskell
< import PGF hiding (Tree)
---
> import PGF2 hiding (Tree)
>
> showCId :: CId -> String
> showCId = id
```

So the module that is imported is now PGF2 instead of PGF, and the function showCId is added, because in PGF2, the type `CId` is actually just an alias for `String`, not a separate type like in the PGF module.

## Differences to ../ReflTransfer.hs

How about the file that we wrote to do the transfer? Here I gave it a different name, and you can run the `diff` command again to see for yourself.

```bash
diff ../ReflTransfer.hs ReflTransferWithPGF2.hs
```

None of the tree transformation functions are different, only the ones using the PGF(2) library. Some of the type signatures are different:

- [`parse` in PGF](https://hackage.haskell.org/package/gf-3.11/docs/PGF.html#g:11):
   ```haskell
   parse :: PGF -> Language -> Type -> String -> [Tree]
   ```

- [`parse` in PGF2](https://hackage.haskell.org/package/pgf2-1.3.0/docs/PGF2.html#g:12):
   ```haskell
   parse :: Concr -> Type -> String -> ParseOutput [(Expr, Float)]
   ```

If you look at the type signatures carefully, you'll notice that the first two arguments to `PGF.parse` (PGF and Language) correspond to the single first arguments to `PGF2.parse` (Concr). So the first step in PGF2 is to extract a Concr using the function [`PGF2.languages`](https://hackage.haskell.org/package/pgf2-1.3.0/docs/PGF2.html#t:ConcName) to get a map, and then accessing the map with the language name, which is just a String.


## Documentation for PGF2

Both of the following are somewhat incomplete, but still useful.

- [API on Hackage](https://hackage.haskell.org/package/pgf2-1.3.0/docs/PGF2.html)
- [Krasimir's guide on GF website](https://www.grammaticalframework.org/doc/runtime-api.html#haskell)

If you have any questions about things that aren't documented, feel free to can ask on [Stack Overflow](https://stackoverflow.com/questions/tagged/gf), [GF Discord server](https://discord.gg/EvfUsjzmaz) or [GF mailing list](https://groups.google.com/group/gf-dev).