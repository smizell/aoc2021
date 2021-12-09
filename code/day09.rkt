#lang racket

(require rackunit
         threading)

(define (load-map filename)
  (~>> filename
       file->string
       (string-split _ "\n")
       (map (λ (l) (~>> l string->list (map string) (map string->number))))))

(struct $pos (x y))

(define (adjacent-pos x y)
  (list (list (sub1 x) y)
        (list (add1 x) y)
        (list x (sub1 y))
        (list x (add1 y))))

(define (on-map? heightmap x y)
  (cond
    [(or (negative? x) (negative? y)) #f]
    [(or (> x (sub1 (length (first heightmap)))) (> y (sub1 (length heightmap)))) #f]
    [else #t]))

(define (pos->value heightmap x y)
  (~> heightmap (list-ref y) (list-ref x)))

(define (find-lowest heightmap)
  (for/fold ([acc '()])
            ([(vs y) (in-indexed heightmap)])
    (for/fold ([acc acc])
              ([(v x) (in-indexed vs)])
      (define is-smallest
        (~>> (adjacent-pos x y)
             (filter (match-lambda [(list ax ay) (on-map? heightmap ax ay)]))
             (map (match-lambda [(list ax ay) (pos->value heightmap ax ay)]))
             (andmap (curry < v))))
      (if is-smallest (cons v acc) acc))))

(define (part1 filename)
  (~>> filename
       load-map
       find-lowest
       (map add1)
       (apply +)))

(module+ test
  (check-eq? (part1 "../inputs/day09.txt") 458))