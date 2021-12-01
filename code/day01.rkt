#lang racket

(define example (list 199
                      200
                      208
                      210
                      200
                      207
                      240
                      269
                      260
                      263))

(define inputs (file->list "../inputs/day01.txt"))

(define (puzzle1 inputs)
  (define-values (r _)
    (for/fold ([acc 0]
               [last #f])
              ([input (in-list inputs)])
      (cond
        [(eq? last #f) (values acc input)]
        [(> input last) (values (add1 acc) input)]
        [else (values acc input)])))
  r)

(define (puzzle2 inputs)
  (let loop ([acc 0]
             [curr inputs]
             [last #f])
    (match curr
      [(list a b c _ ...) (define t (+ a b c))
                          (define r (rest curr))
                          (cond
                            [(eq? last #f) (loop acc r t)]
                            [(> t last) (loop (add1 acc) r t)]
                            [else (loop acc r t)])]
      [_ acc])))

(puzzle1 inputs)
(puzzle2 inputs)