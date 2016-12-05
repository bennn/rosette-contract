#lang racket/base
(require rosette-contract)
(require racket/class)

(provide
  stack/c
  env/c
  val/c
  state/c
  result/c
  command%/c

  ann
  assert)

(define-syntax-rule (ann e t)
  e)

(define-syntax-rule (assert e p)
  (let ([v e])
    (unless (p v)
      (raise-user-error 'assert))
    v))

(define command%/c
  (class/c
    (init-field
      #;(binop (-> integer? integer? integer?))
      (descr string?))
    (field
      #;(binop (-> integer? integer? integer?))
      (descr string?)
      )))

(define stack/c (listof integer?))
(define env/c (listof (instanceof/c command%/c)))
(define val/c (or/c symbol? integer?))
(define state/c stack/c)
(define result/c (or/c 'EXIT #f (cons/c env/c state/c)))

#;(define-type Command%
  (Class
    (init-field
      [id Symbol]
      [descr String]
      [exec (-> Env State Any Result)])))

