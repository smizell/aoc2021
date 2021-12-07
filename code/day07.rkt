#lang racket

(require threading)

(define (load-pos filename)
  (~>> filename file->string string-trim (string-split _ ",") (map string->number)))

(define (calculate ps [modifier identity])
  (for/lists (cs #:result (apply min cs))
             ([n (in-range (apply min ps)
                           (add1 (apply max ps)))])
    (for/lists (ds #:result (apply + ds))
               ([p (in-list ps)])
      (modifier (abs (- n p))))))

(define (part1 filename)
  (calculate (load-pos filename)))

(define (part2 filename)
  (calculate (load-pos filename)
             (λ (n) (/ (* n (+ n 1)) 2))))