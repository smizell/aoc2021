#lang racket

(require threading)

(define example-filename "../inputs/day06-example.txt")
(define input-filename "../inputs/day06.txt")

(define (load-fish filename)
  (~>> filename file->string string-trim (string-split _ ",") (map string->number)))

(define (tick-fish fish)
  (for/fold ([acc '()])
            ([f (in-list fish)])
    (match f
      [0 (cons 6 (cons 8 acc))]
      [_ (cons (sub1 f) acc)])))

(define (simulate fish days)
  (cond
    [(zero? days) fish]
    [else (simulate (tick-fish fish) (sub1 days))]))

(define (tick-fish* fish)
  (for/fold ([acc (hash)])
            ([(d c) (in-hash fish)])
    (match d
      [0 (~> acc
             (hash-set 6 (+ (hash-ref acc 6 0) c))
             (hash-set 8 (+ (hash-ref acc 8 0) c)))]
      [_ (hash-set acc (sub1 d) (+ (hash-ref acc (sub1 d) 0) c))])))

(define (simulate* fish days)
  (cond
    [(zero? days) (apply + (hash-values fish))]
    [else (simulate* (tick-fish* fish) (sub1 days))]))

(define (fish->hash fish)
  (for/fold ([acc (hash)])
            ([f (in-list fish)])
    (hash-update acc f add1 0)))

(define fish (~> input-filename load-fish fish->hash))
(simulate* fish 80) ; part 1
(simulate* fish 256) ; part 2