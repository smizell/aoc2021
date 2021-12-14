#lang racket

(require threading)

(define (part1 filename)
  (run-polymer (load-manual filename) 10))

(define (run-polymer m r)
  (define ts
    (for/fold ([m m] #:result ($manual-template m))
              ([i (in-range r)])
      (step m)))
  (define cs
    (for/fold ([c (hash)] #:result (hash-values c))
              ([t (in-list ts)])
      (hash-update c t add1 0)))
  (- (apply max cs) (apply min cs)))

(struct $manual (template rules) #:transparent)

(define (load-manual filename)
  (match-define (list template rules) (~> filename file->string (string-split "\n\n")))
  (define template* (~>> template string->list (map string)))
  (define rules*
    (for/fold ([acc (hash)])
              ([r (in-list (string-split rules "\n"))])
      (match r
        [(pregexp #px"(\\w)(\\w) -> (\\w)" (list _ a b c))
         (hash-set acc (list a b) c)])))
  ($manual template* rules*))

(define (apply-rule p rs)
  (list (first p) (hash-ref rs p)))

(define (step manual)
  (match-define ($manual t rs) manual)
  (define nt
    (let loop ([t t])
      (cond
        [(= 1 (length t)) t]
        [else (define p (take t 2))
              (append (apply-rule p rs) (loop (cdr t)))])))
  ($manual nt rs))
