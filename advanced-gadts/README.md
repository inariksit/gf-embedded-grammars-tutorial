# Haskell abstract syntax using GADTs

This example uses the native Haskell PGF library, but as opposed to the simplest example, we use the flag `--haskell=gadt`.

The file `MiniLang.hs` in this directory has been generated automatically with the following command.

```bash
gf -make -f haskell --haskell=gadt --haskell=lexical --lexical=A,Adv,Conj,Det,N,PN,Pron,Prep,V,V2 ../resource/MiniLangEng.gf
```

The differences between the files are pretty significant. Instead of mutually recursive datatypes from [../MiniLang.hs](../MiniLang.hs) and [../advanced-pgf2/MiniLang.hs](../advanced-pgf2/MiniLang.hs), now the whole `Tree` is a single GADT, parameterized by