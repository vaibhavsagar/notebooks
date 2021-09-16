0.4

* Generalize `Diff a` to `PolyDiff a b`.  `Diff` has been replaced with
a specialized synonym `type Diff a = PolyDiff a a`, but it's still not
backward compatible if you imported `Diff(..)`.
