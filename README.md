# haxcs


[![DOI](https://zenodo.org/badge/611736447.svg)](https://zenodo.org/badge/latestdoi/611736447)


Haskell implementation of the XCS classifier system. This was described in the
2018 IWLCS paper [An Algebraic Description of
XCS](https://doi.org/10.1145/3205651.3208248) by Pätzel and Hähner. As of 2023,
the implementation accompanying the paper had so far not been published which
was mainly due to my own research focus shifting away from investigating XCS and
me not having the time to properly clean up the implementation. I'm now
releasing the code in the state as it is—true to the motto “suboptimal code is
better than no code”.


Note that in the months after publishing above-mentioned paper, some changes
have been made to the implementation which now slightly differs from the one
reported on there:

- I switched from literate to non-literate Haskell for an improved
  maintainability.
- `MonadSystem` was renamed to `MonadEnvironment`.
- `generalises` was renamed to `generalizes`
- `Condition` expects a definition of `generality`
- `Metadata` records have been renamed such that they're closer to the original
  names.
- `Control.Lens` is used in many places for easier access to and manipulation of
  fields.
- `Population` has been made abstract by means of introducing a `Storage` class
  as well as a `Data.Map.Strict`-based implementation of it.
- An attempt at unifying the interface for single- and multistep tasks.
- Possibly more changes that I don't recall or did not find in the code right
  now.


## Running the code


I strongly recommend to use the [Nix](https://nixos.org/) package manager. If you do, first enable [Flake support](https://nixos.wiki/wiki/Flakes) and then simply run


```
nix develop
```


which drops you in a shell where you can run the code like so:


```
cabal run haxcs -- -s 30000 -r 3 -p Multiplexer11
```


Note that running the code current produces a bunch of PDFs in the current
working directory. The file names are hardcoded in the `Plot` module.


## Citing this


If you build on this work, please cite the 2018 IWLCS paper [An Algebraic Description of XCS](https://doi.org/10.1145/3205651.3208248) by Pätzel and Hähner as well as this implementation itself (it should get assigned a DOI by Zenodo).
