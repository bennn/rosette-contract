rosette-contract
===

Rosette front-end to `racket/contract`.
Uses `z3` to prove some (parts) of contracts unnecessary.

TODO
---
- try soft-contracts
- try generating
- try solving for lists, etc
- where is time going?
- what contracts do we win on?
- winning on stronger?
- how to extend "solvable?"
  - for now, used Rosette's solvable
  - but really you could compile a query / asserts for [Listof solvable?]

- presentation
  - reflections on rosette


etc
---

"We could also imagine a third, ungainly semantics that looks into closures
 rather than having explicit function proxies." -mgree

racket/contract docs bugs
- under `make-flat-contract`
  - flat-contract-with-reason broken link
  - contract-first-order-okay-to-give-up? broken link
  - "it's doc"
  (no idea why these are broken, guess ask robby)

