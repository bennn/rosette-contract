private
===

Back-end for Rosette-solvable contracts

Index
---

- `arrow.rkt` Rosette-solvable function contracts
- `base.rkt` define structure type for Rosette-solvable contracts,
             the trivial solvable contract
- `combinator.rkt` solvable contract combinators, e.g. `or/c`
- `env/` define Rosette versions of flat contracts,
         and re-export identifiers from Rosette and Racket
- `flat.rkt` Rosette-solvable flat contracts (predicates)
- `out.rkt` attaching Rosette-solvable contracts to values
- `solve.rkt` simplify Rosette-solvable contracts, run the SMT solver
- `util/` library utilities (logging, parameters)
