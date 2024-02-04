let
  lock = builtins.fromJSON (builtins.readFile ./flake.lock);
  versions = builtins.mapAttrs
    (_: node: (builtins.fetchTree node.locked).outPath)
    lock.nodes;
in versions
