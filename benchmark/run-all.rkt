#lang racket/base

;; Script to run benchmarks & collect results

;; Usage:
;;   racket run-all.rkt <DIR> ...
;; Search every directory <DIR> for files called `main.rkt`.
;; Run each `main.rkt` with `racket/contract` and with `rosette-contract`.
;; Print information about the number of contracts, compile time, runtime, etc.

(provide
)

(require
  file/glob
  racket/file
  racket/runtime-path
  racket/string
  racket/system
  (only-in racket/match
    match-define)
  (only-in racket/list
    first second third fourth fifth
    last
    add-between)
  (only-in racket/path
    shrink-path-wrt)
  (only-in racket/port
    port->string)
)

;; =============================================================================

(define *output-file* (make-parameter #f))
(define *racket-version* (make-parameter "6.7"))

(define (log-info msg . arg*)
  (display "INFO: ")
  (apply printf msg arg*)
  (newline))

(define (log-debug msg . arg*)
  (display "DEBUG: ")
  (apply printf msg arg*)
  (newline))

;; -----------------------------------------------------------------------------
;; type Row = (make-row N N N N N N N N N)

(struct row (
  contract-total-time ;;  Natural, total time to evaluate definitions & run
  contract-running-time ;; Natural, total time to run file (after defns.)
  contract-compile-time ;; Natural
  rosette-total-time ;; Natural, total time to evaluate definitions & run file
  rosette-running-time ;; Natural, total time to run file (after defns.)
  rosette-compile-time ;; Natural
  smt-success ;; (Listof String), smt successes (simplified contracts)
  smt-failure ;; (Listof String), smt failures (not simplified contracts)
  contract* ;; (Listof String), contract definitions

  ;; (add more columns later?)
)
#:transparent
#:mutable
#:extra-constructor-name make-row
)

(define (row-init)
  (make-row 0 0 0 0 0 0 0 0 0))

;; assert-zero! : Row (-> Row Natural) -> Void
(define (assert-zero! R f)
  (unless (zero? (f R))
    (log-warning "expected 0 value at '~a' in ~a" (object-name f) R)))

;; assert-zero!* : Row (-> Row Natural) * -> Void
(define (assert-zero!* R . f*)
  (for ([f (in-list f*)])
    (assert-zero! R f)))

;; -----------------------------------------------------------------------------
;; type Summary = { Path-String => Row }

;; summary-init : -> Summary
(define (summary-init)
  (make-hash))

;; summary-add : Summary Path-String Row -> Summary
(define (summary-add S ps r)
  (define old (hash-ref S ps #f))
  (when old
    (log-warning "overwriting existing entry for '~a'" old))
  (hash-set! S ps r)
  S)

;; -----------------------------------------------------------------------------

;; run/racket : Path-String -> Row
(define (run/racket f)
  (define-values (dir file) (file-path->dir+file f))
  (define tmp (make-temp-dir dir))
  (racket/contract! tmp)
  (define R
    (parameterize ([current-directory tmp])
      (run file 'contract)))
  (delete-directory/files tmp)
  (assert-zero!* R row-rosette-total-time row-rosette-running-time row-rosette-compile-time)
  R)

;; run/rosette : Path-String -> Row
(define (run/rosette f)
  (define-values (dir file) (file-path->dir+file f))
  (parameterize ([current-directory dir])
    (run file 'rosette)))

;; run : Path-String (or/c 'contract 'rosette) -> Row
(define (run pre-f mode)
  (define f (if (path? pre-f) (path->string pre-f) pre-f))
  (cond
   [(file-exists? f)
    ;; am in current directory,
    ;; just execute file, subscribed to the logger,
    ;; and capture output
    ;; - can use time-apply, over call to system
    (define (thunk)
      (define info (make-hasheq '((running-time . 0) (compile-time . 0)
                                  (smt-success . ()) (smt-failure . ())
                                  (contract* . ()))))
      (log-info "processing '~a' ..." f)
      (log-debug "- deleting compiled files")
      (system "find . -name 'compiled' | xargs rm -r")
      (log-debug "- recompiling")
      (hash-set! info 'compile-time
        (measure-cpu-time system (format "raco~a make '~a'" (*racket-version*) f)))
      (log-debug "- running")
      (define-values (total-time out in pid err control)
        (let ([cmd (format "racket~a -W debug@rosette-contract '~a'" (*racket-version*) f)])
          (let-values ([(r* cpu-time real-time gc-time)
                        (time-apply
                          (λ ()
                            #;(printf "[[ invoking subprocess ]]~n")
                            (define r* (process cmd #:set-pwd? #t))
                            (define control (fifth r*))
                            (let loop ()
                              (case (control 'status)
                               [(running)
                                #;(printf "[[ waiting on subproc ]]~n")
                                (control 'wait)
                                (loop)]
                               [(done-ok)
                                (void)]
                               [(done-error)
                                (define exn-str (port->string (fourth r*)))
                                (close-input-port (first r*))
                                (close-output-port (second r*))
                                (close-input-port (fourth r*))
                                (raise-user-error 'run "system command '~a' raised an exception~n~a" cmd exn-str)]))
                            #;(printf "[[ done waiting ]]~n")
                            (apply values r*)) '())])
            (apply values cpu-time r*))))
      #;(hash-set! info 'running-time total-time)
      (log-debug "- processing output")
      (for ([ln (in-sequences (in-lines out) (in-lines err))])
        (cond
         [(string-prefix? ln "cpu time: ")
          (hash-set! info 'running-time
            (string->number (second (regexp-match #rx"^cpu time: ([0-9]+) real" ln))))]
         [(regexp-match? #rx"SUCCESS" ln)
          (hash-set! info 'smt-success
            (cons ln (hash-ref info 'smt-success)))]
         [(regexp-match? #rx"FAILURE" ln)
          (hash-set! info 'smt-failure
            (cons ln (hash-ref info 'smt-failure '())))]
         [(regexp-match? #rx"ATTACH define/contract" ln)
          (hash-set! info 'contract*
            (cons ln (hash-ref info 'contract* '())))]
         [else
          (void)]))
      (close-input-port out)
      (close-output-port in)
      (close-input-port err)
      info)
    (define-values (r* cpu-time real-time gc-time) (time-apply thunk '()))
    (define total-time real-time)
    (define H (car r*))
    (case mode
     [(contract)
      (make-row
        total-time
        (hash-ref H 'running-time)
        (hash-ref H 'compile-time)
        0 0 0
        (hash-ref H 'smt-success)
        (hash-ref H 'smt-failure)
        (hash-ref H 'contract*))]
     [(rosette)
      (make-row
        0 0 0
        total-time
        (hash-ref H 'running-time)
        (hash-ref H 'compile-time)
        (hash-ref H 'smt-success)
        (hash-ref H 'smt-failure)
        (hash-ref H 'contract*))]
     [else
      (raise-user-error 'run "unknown mode '~a'" mode)])]
   [else
    (log-error "file '~a' does not exist in '~a'" f (current-directory))
    (row-init)]))

(define (measure-cpu-time f . x*)
  (define-values (r cpu real gc) (time-apply f x*))
  cpu)

(define (file-path->dir+file f)
  (let-values (((base name mbd?) (split-path f)))
    (values (if (path? base) base (current-directory))
            name)))

(define (make-temp-dir dirname)
  (define temp-dir (find-system-path 'temp-dir))
  (define tag
    (let ([ps (last (explode-path dirname))])
      (string->symbol (if (path? ps) (path->string ps) ps))))
  (define new-dirname
    (let loop ([tag+ (gensym tag)])
      (define p (build-path temp-dir (symbol->string tag+)))
      (if (or (file-exists? p) (directory-exists? p))
        (loop (gensym tag))
        p)))
  (copy-directory/files dirname new-dirname)
  new-dirname)

(define (racket/contract! dir)
  (printf "converting ~a to racket/contract~n" (glob (build-path dir "*.rkt")))
  (for ([fn (in-glob (build-path dir "*.rkt"))])
    (define fn-tmp (path-replace-extension fn ".rkt.tmp"))
    (with-output-to-file fn-tmp #:exists 'error
      (λ ()
        (with-input-from-file fn
          (λ ()
            (define success?
              (for/fold ([replaced? #f])
                        ([ln (in-lines)])
                (cond
                 [(and (not replaced?)
                       (regexp-match? #rx"\\(require *rosette-contract\\)" ln))
                  (displayln "(require racket/contract)")]
                 [else
                  (displayln ln)])))
            (unless success?
              (log-error "no import for rosette-contract in ~a" fn))))))
    (copy-file fn-tmp fn 'exists-ok)
    (delete-file fn-tmp)
    (void)))

;; -----------------------------------------------------------------------------

;; collect-file : Path-String -> Row
(define (collect-file f)
  (define rkt (run/racket f))
  (define ros (run/rosette f))
  (make-row
    (row-contract-total-time rkt)
    (row-contract-running-time rkt)
    (row-contract-compile-time rkt)
    (row-rosette-total-time ros)
    (row-rosette-running-time ros)
    (row-rosette-compile-time ros)
    (row-smt-success ros)
    (row-smt-failure ros)
    (row-contract* ros)))

;; collect-results : [Listof Source] -> Summary
(define (collect-results file-or-dir*)
  (printf "collect-results ~a~n" file-or-dir*)
  (for/fold ([S (summary-init)])
            ([fd (in-list file-or-dir*)])
    (cond
     [(file-exists? fd)
      (summary-add S fd (collect-file fd))]
     [(directory-exists? fd)
      (for/fold ([S S])
                ([f (in-glob (build-path fd "main.rkt"))])
        (summary-add S f (collect-file f)))]
     [else
      (log-warning "skipping invalid data source '~a'" fd)
      S])))

;; print-results : (or/c #f path-string?) Summary -> Void
(define (print-results p S)
  (define (print)
    (print-title)
    (for ([(fn r) (in-hash S)])
      (displayln
        (string*->org-row (cons (abbreviate fn) (row->string* r)))))
    (void))
  (cond
   [(not p)
    (print)]
   [(output-port? p)
    (parameterize ([current-output-port p])
      (print))]
   [(path-string? p)
    (with-output-to-file p #:exists 'append
      print)]))

(define (abbreviate ps)
  (let loop ([ps* (explode-path ps)]
             [cwd* (explode-path CWD)])
    (cond
     [(or (null? cwd*) (eq? 'same (car cwd*)))
      (path->string (apply build-path ps*))]
     [(or (null? ps*) (not (string=? (path->string (car ps*)) (path->string (car cwd*)))))
      ps]
     [else
      (loop (cdr ps*) (cdr cwd*))])))

(define (row->string* r)
  (map number->string
    (list (row-contract-total-time r)
          (row-contract-running-time r)
          (row-contract-compile-time r)
          (row-rosette-total-time r)
          (row-rosette-running-time r)
          (row-rosette-compile-time r)
          (length (row-smt-success r))
          (length (row-smt-failure r))
          (length (row-contract* r)))))

(define (print-title)
  (displayln
    (string*->org-row
      '("filename"
        "contract total (ms)" "contract run (ms)" "contract compile (ms)"
        "rosette total (ms)"  "rosette run (ms)"  "rosette compile (ms)"
        "#smt success" "#smt failure"
        "#contract"))))

(define (string*->org-row str*)
  (string-join str* " | "
    #:before-first "| "
    #:after-last " |"))

;; =============================================================================

(define-runtime-path CWD ".")

(module+ main
  (require racket/cmdline)
  (command-line
   #:program "rosette-contract-benchmark"
   #:once-each
   [("-o" "--output") FILE "Print extra info. to FILE" (*output-file* FILE)]
   ;; -version
   #:args SRC*
   (print-results (*output-file*) (collect-results (if (null? SRC*) (directory-list CWD) SRC*))))
)

;; -----------------------------------------------------------------------------

(module+ test
  (require rackunit)
)
