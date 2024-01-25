# Tutorial: How to use GF grammars from another program

This repository contains code examples to complement my blog post on [embedding grammars](https://inariksit.github.io/gf/2019/12/12/embedding-grammars.html). For general introduction, here's the intro of the blog post.

> This post will show how to use GF grammars from an external program, and how to manipulate GF trees from that program.
> The topic is introduced in [Lesson 7](http://www.grammaticalframework.org/doc/tutorial/gf-tutorial.html#toc143) of the tutorial, and I will cover parts that I find missing in the tutorial:

> * Installation, other practicalities of the GF ecosystem
> * Linguistically motivated example of tree manipulation
> * Examples in both Haskell and Python (the examples are the same, so only knowing one is enough)


## Advanced topics

These topics are discussed in [the sequel blog post](https://inariksit.github.io/gf/2024/01/25/embedding-grammars-2.html). To see full self-contained examples, each with their own README file, go to the following directories.

* Haskell bindings to the C runtime: [advanced-pgf2](advanced-pgf2) -- Minimal difference to the "basic" example
* GADTs in the generated Haskell code: [advanced-gadts](advanced-gadts) -- Significant difference, a new way of doing tree transformations
