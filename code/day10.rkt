#lang racket

(require threading
         rackunit)

(define (load-code filename)
  (~>> filename
       file->string
       (string-split _ "\n")
       (map (λ (l) (~>> l string->list (map string))))))

(define chars (hash  "{" "}"
                     "(" ")" 
                     "<" ">"
                     "[" "]"))

(define (opening? value)
  (hash-has-key? chars value))

(define (closing? value)
  (if (member value (hash-values chars)) #t #f))

(define (closing-for? opening closing)
  (if (equal? (hash-ref chars opening "") closing) #t #f))

(define (parse-line line [current '()])
  (cond
    [(and (not (empty? current)) (empty? line)) (list 'incomplete current line)]
    [(empty? line) (list 'done)]
    [(empty? current) (parse-line (rest line) (cons (first line) current))]
    [(closing-for? (first current) (first line)) (parse-line (rest line) (rest current))]
    [(opening? (first line)) (parse-line (rest line) (cons (first line) current))]
    [else (list 'error (first line) line)]))

(define (part1 filename)
  (~>> filename
       load-code
       (map parse-line)
       (filter (λ (l) (match l
                        [(cons 'error _) #t]
                        [_ #f])))
       (map (λ (e) (match (second e)
                     [")" 3]
                     ["]" 57]
                     ["}" 1197]
                     [">" 25137])))
       (apply +)))

(define (complete-line incomplete)
  (map (λ (c) (hash-ref chars c)) incomplete))

(define (part2 filename)
  (define results
    (~>> filename
         load-code
         (map parse-line)
         (filter (λ (l) (match l
                          [(cons 'incomplete _) #t]
                          [_ #f])))
         (map (λ (l) (complete-line (second l))))
         (map (λ (cs)
                (for/fold ([acc 0])
                          ([c (in-list cs)])
                  (+ (* acc 5)
                     (match c
                       [")" 1]
                       ["]" 2]
                       ["}" 3]
                       [">" 4])))))
         (sort _ >)))
  (list-ref results (floor (/ (length results) 2))))

(module+ test
  (check-equal? (part2 "../inputs/day10.txt") 820045242))