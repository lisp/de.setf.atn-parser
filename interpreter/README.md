# ATN interpreter

`interpret-atn-system` executes the canonical `atn-system` produced by
`bnf-to-atn` without generating or loading Lisp source.

```lisp
(let ((system (bnf-to-atn grammar)))
  (interpret-atn-system system tokens
                        :start-name 'expression
                        :mode :multiple
                        :reduce t))
```

It returns the same three values as a generated parser: the reduced value, the
ending input index, and whether the parse consumed the complete input.

The interpreter supports the `word`, `cat`, `test`, `push`, `jump`, `or`,
`pop`, and `fail` ATN edges. `:input-function`, `:input-eof-function`,
`:word-predicate`, `:register-words`, and `:reduce` have the same roles as in
`compile-atn-system`.

With `:reduce t`, terminal constructors receive the input item and production
constructors receive one positional argument for each production term.
Constructor specializers are passed as the leading context argument. With
`:reduce 'cons`, parse trees are built without calling constructors.

`make-atn-interpreter` returns a parser closure when a reusable, generated
parser-like function is more convenient.

The focused smoke tests are kept out of the production ASDF system:

```lisp
(load "interpreter/tests.lisp")
(run-atn-interpreter-tests)
```

