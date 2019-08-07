# notebooks

This is a repository of my IHaskell notebooks, provisioned using Nix. To run
one of them, e.g. `continuations`, you can do this:

```bash
$ cd continuations
$ $(nix-build)/bin/ihaskell-notebook
<...>
```

To speed up building, you can use my cache at https://vaibhavsagar.cachix.org.
