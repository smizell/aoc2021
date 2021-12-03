#lang racket

(require threading
         math/matrix)

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
       (map (λ (ns) (~>> ns string->list (map string) (map string->number))))))


(define inputs
  (call-with-input-file "../inputs/day03.txt"
    (λ (in)
      (for/list ([line (in-lines in)])
        (~>> line string->list (map string) (map string->number))))))

(define (binary->decimal n)
  (cond
    [(zero? n) n]
    [else (+ (modulo n 10) (* 2 (binary->decimal (quotient n 10))))]))

(define (num-list->decimal l)
  (~>> l
       (map number->string)
       (string-join _ "")
       string->number
       binary->decimal))

(define (transpose nss)
  (apply map list nss))

(define (bits->rates ns)
  (for/fold ([zs 0]
             [os 1]
             ; (list gamma epsilon)
             #:result (if (> zs os) (list 0 1) (list 1 0)))
            ([n (in-list ns)])
    (if (= 0 n) (values (add1 zs) os) (values zs (add1 os)))))

(define (part1 inputs)
  (~>> inputs
       transpose
       (map bits->rates)
       transpose
       (map num-list->decimal)
       (apply *)))