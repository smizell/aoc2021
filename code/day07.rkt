#lang racket

(require threading)

(define (load-pos filename)
  (~>> filename file->string string-trim (string-split _ ",") (map string->number)))

(define (calculate ps fn)
  (for/lists (cs #:result (apply min cs))
             ([n (in-range (apply min ps)
                           (add1 (apply max ps)))])
    (for/lists (ds #:result (apply + ds))
               ([p (in-list ps)])
      (fn p n))))

(define (part1 filename)
  (calculate (load-pos filename)
             (λ (a b) (abs (- a b)))))

(define (part2 filename)
  (calculate (load-pos filename)
             (λ (a b)
               (define n (abs (- a b)))
               (/ (* n (+ n 1)) 2))))