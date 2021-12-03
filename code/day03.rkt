#lang racket

(require threading)

(define (parse-line l)
  (~>> l string->list (map string) (map string->number)))

(define example
  (~>> (list "00100"
             "11110"
             "10110"
             "10111"
             "10101"
             "01111"
             "00111"
             "11100"
             "10000"
             "11001"
             "00010"
             "01010")
       (map parse-line)))

(define inputs
  (call-with-input-file "../inputs/day03.txt"
    (λ (in)
      (for/list ([line (in-lines in)])
        (parse-line line)))))

(define (binary->decimal n)
  (cond
    [(zero? n) n]
    [else (+ (modulo n 10) (* 2 (binary->decimal (quotient n 10))))]))

(define (num-list->decimal nl)
  (~>> nl
       (map number->string)
       (string-join _ "")
       string->number
       binary->decimal))

(define (transpose nss)
  (apply map list nss))

(define (zero-count l)
  (count (λ (n) (= n 0)) l))

(define (one-count l)
  (count (λ (n) (= n 1)) l))

(define (order-bits-by-count ns)
  (if (> (zero-count ns) (one-count ns))
      (list 0 1)
      (list 1 0)))

(define (get-gamma-epsilon nss)
  (~>> nss
       transpose
       (map order-bits-by-count)
       transpose))

(define (part1 inputs)
  (~>> inputs
       get-gamma-epsilon
       (map num-list->decimal)
       (apply *)))