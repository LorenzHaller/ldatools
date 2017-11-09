# ldatools 0.0.326

* Rewrote `split_info` function, removing the dependency on
`survival::survSplit` and allowing for left closed, right open intervals

* `lifetable` function now used correct, left closed intervals

* both functions can be used with bare, unquoted variable names
