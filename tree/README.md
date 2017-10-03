# lark/tree

> “Leaves turned to soil beneath my feet. Thus it is, trees eat themselves.”
>
> -- _David Mitchell, Cloud Atlas_

**lark/tree** is a tool for representing and manipulating heterogeneous forms of information as editable, traversible trees.

It is already useful and used in production, but you should not rely on it for your own applications as it remains in flux, changing frequently and dramatically, with no commitment to any existing public api.

## What can it do?

1. Parse raw Clojure source into an AST (`lark.tree.core/ast`) and corresponding zipper (`lark.tree.core/ast-zip`).
2. Traverse and edit as desired.
3. Emit Clojure (`lark.tree.core/string` or `lark.tree.core/sexp`) with existing whitespace intact.

An example of real-world usage can be found in [Maria](https://github.com/mhuebert/maria), a beginner-friendly ClojureScript REPL.

## Testing

`lein doo phantom test`
