benchmark
===

Benchmarks / stress tests for the `rosette-contract` library.

###### To run a single benchmark

Compile and run any `main.rkt` file.
E.g.

```
$ raco make <DIR>/main.rkt
$ racket -W debug@rosette-contract <DIR>/main.rkt
```

Notes:
- The `rosette-contract` logger recieves events about solver queries and reduced contracts.
- Every `.rkt` file in every benchmark uses the `rosette-contract` library.
  Replace `rosette-contract` with `racket/contract` to see the baseline performance.


###### To run all benchmarks

```
$ racket run-all.rkt
```

This will print as tests run, and summarize the results to STDOUT.


About the Benchmarks
---

#### shootout

Tests from the shootout benchmarks.

Online here:
https://github.com/racket/racket/tree/master/pkgs/racket-benchmarks/tests/racket/benchmarks/shootout

Some already-contracted benchmarks here:
https://github.com/LeifAndersen/experimental-methods-in-pl/tree/master/shootout


#### gtp

Tests from the `gradual-typing-performance` repo:
https://github.com/nuprl/gradual-typing-performance


TODO
---

- [ ] add a "lint" script, to check that benchmarks are well-formed?
  - with a `main.rkt`
  - that compiles/runs okay
  - every file requires `rosette-contract` and not `racket/contract`
  - every file in `#lang racket/base`
  - ???
