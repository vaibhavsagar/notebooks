# notebooks

This is a repository of my [IHaskell](https://github.com/gibiansky/IHaskell)
notebooks, provisioned using [Nix](https://nixos.org/). To run one of them,
e.g. `continuations`, you can do this:

```bash
$ cd continuations
$ $(nix-build)/bin/jupyter-notebook
<...>
```

To speed up building, you can use my cache at https://vaibhavsagar.cachix.org.
