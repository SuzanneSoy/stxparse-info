stxparse-info
=============

The module `stxparse-info/parse` is a patched version of `syntax/parse` which
tracks which syntax pattern variables are bound. This allows some libraries to
change the way syntax pattern variables work.

For example, `phc-graph/subtemplate` automatically derives temporary
identifiers when a template contains `yᵢ …`, and `xᵢ` is a pattern
variable. To know from which `varᵢ` the `yᵢ …` identifiers must be derived,
`phc-graph/subtemplate` needs to know which syntax pattern variables are
within scope.

To use this package, simply require `stxparse-info/pares` instead of
`syntax/parse` Additionally, it is possible to use the for-syntax function
`(current-pvars)` from `stxparse-info/current-pvars` to access the list of
currently-bound pattern variables, and the macro `(with-pvars (pvar ...)
. body)` can be used to add new pattern variables to the list.