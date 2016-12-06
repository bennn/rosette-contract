#lang racket/base

(require rosette)

(provide
  ;; -- 3.8 procedure expressions
  lambda
  ;; -- 3.9 local binding
  let let* letrec let-values let*-values letrec-values
  ;; -- 3.12 conditionals
  if cond and or
  ;; -- 3.15 sequencing
  begin
  ;; -- 3.16 guarded evaluation
  when unless
  ;; -- 4.1 booleans and equality
  < <= = >= >
  ;; -- 4.2 numbers
  expt + - * / add1 sub1
  zero?
  ;; -- 4.3 strings byte strings characters
  string->number
  ;; -- 4.6 symbols
  ;; -- 4.7 regular expressions
  ;; -- 4.8 keywords
  ;; -- 4.9 pairs and lists
  empty empty? make-list reverse car cdr first second third fourth fifth sixth rest
  ;; -- 4.10 mutable pairs and lists
  ;; -- 4.11 vectors
  vector-ref vector-set! make-vector
  ;; -- 4.12 boxes
  ;; -- 4.13 hash tables
  ;; -- 4.14 sequences
  ;; -- 4.16 sets
  ;; -- 4.17 procedures
  ;; -- 4.18 void
  ;; -- 10.2 exceptions
  with-handlers
)

