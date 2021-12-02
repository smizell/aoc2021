#lang racket

(define example
  `((forward 5)
    (down 5)
    (forward 8)
    (up 3)
    (down 8)
    (forward 2)))

(define inputs
  (call-with-input-file "../inputs/day02.txt"
    (λ (in)
      (for/list ([line (in-lines in)])
        (define parts (string-split line))
        (list (string->symbol (first parts))
              (string->number (second parts)))))))

(define (part1 inputs)
  (let loop ([cs inputs]
             [hp 0]
             [dp 0])
    (match cs
      [`((forward ,u) ,rcs ...) (loop rcs (+ hp u) dp)]
      [`((down ,u) ,rcs ...) (loop rcs hp (+ dp u))]
      [`((up ,u) ,rcs ...) (loop rcs hp (- dp u))]
      ['() (* hp dp)])))

(define (part1a inputs)
  (for/fold ([hp 0]
             [dp 0]
             #:result (* hp dp))
            ([input (in-list inputs)])
    (match input
      [`(forward ,u) (values (+ hp u) dp)]
      [`(down ,u) (values hp (+ dp u))]
      [`(up ,u) (values hp (- dp u))])))

(part1 inputs)

(define (part2 inputs)
  (let loop ([cs inputs]
             [hp 0]
             [dp 0]
             [aim 0])
    (match cs
      [`((forward ,u) ,rcs ...) (loop rcs (+ hp u) (+ dp (* aim u)) aim)]
      [`((down ,u) ,rcs ...) (loop rcs hp dp (+ aim u))]
      [`((up ,u) ,rcs ...) (loop rcs hp dp (- aim u))]
      [`() (* hp dp)])))

(part2 inputs)