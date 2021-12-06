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
  (match days
    [0 fish]
    [_ (simulate (tick-fish fish) (sub1 days))]))

(define (increase-hash-value h k v [default 0])
  (hash-update h k (curry + v) default))

(define (tick-fish* fish)
  (for/fold ([acc (hash)])
            ([(d c) (in-hash fish)])
    (match d
      [0 (~> acc (increase-hash-value 6 c) (increase-hash-value 8 c))]
      [_ (increase-hash-value acc (sub1 d) c)])))

(define (simulate* fish days)
  (match days
    [0 (apply + (hash-values fish))]
    [_ (simulate* (tick-fish* fish) (sub1 days))]))

(define (fish->hash fish)
  (for/fold ([acc (hash)])
            ([f (in-list fish)])
    (hash-update acc f add1 0)))

(define fish (~> input-filename load-fish fish->hash))
(simulate* fish 80) ; part 1
(simulate* fish 256) ; part 2