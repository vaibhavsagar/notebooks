let
  pkgs    = import ./pkgs.nix;
  nixpkgs = import pkgs.nixpkgs {};
  notebooks = map (folder: {
    name = folder;
    path = import (./. + "/${folder}");
  });
in nixpkgs.linkFarm "notebooks" (notebooks [
  "callcc"
  "chart-diagrams"
  "codensity"
  "continuations"
  "deriving-via"
  "docker"
  "dragon-curve"
  "efficient-combinator-parsers"
  "git-from-scratch"
  "graphviz"
  "hamt"
  "hs-updater"
  "inline-r"
  "intmap"
  "lambda"
  "mph"
  "refactoring-tarjan"
  "revisiting-monadic-parsing-haskell"
  "revisiting-poor-mans-concurrency"
  "smt"
  "solver"
  "tarjan"
  "trees-that-shrink"
  "typeclasses"
  "zulip-api"
])
