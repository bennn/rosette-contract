#lang racket/base
(require racket/contract)

;; Copyright 2014 John Clements (clements@racket-lang.org)
;; Code licensed under the Mozilla Public License 2.0


;; -----------------------------------------------------------------------------

(require
  (only-in racket/file file->value)
  "morse-code-strings.rkt"
  "levenshtein.rkt"
)

(define word-frequency-list "frequency.rktd")
(define word-frequency-list-small "frequency-small.rktd")

(define freq-list? (listof (list/c string? integer?)))

#;(: file->words (-> String (Listof String)))
(define (file->words filename)
  (define words+freqs (file->value (string->path filename)))
  (unless (freq-list? words+freqs) (error "expected a frequency list"))
  (for/list  ([word+freq  words+freqs])
    (car word+freq)))

#;(: allwords (Listof String))
(define allwords (file->words word-frequency-list))

#;(: words-small (Listof String))
(define words-small (file->words word-frequency-list-small))

#;(: main (-> (Listof String) Void))
(define (main words)
  (for* ([w1 (in-list words)]
         [w2 (in-list words)])
    (string->morse w1)
    (string->morse w2)
    (string-levenshtein w1 w2)
    (string-levenshtein w2 w1)
    (void)))

;(time (main allwords)) ;; 68,000ms
(time (main words-small)) ;; 200ms
