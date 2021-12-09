#lang racket

(require rackunit
         threading)

(define (load-map filename)
  (~>> filename
       file->string
       (string-split _ "\n")
       (map (λ (l) (~>> l string->list (map string) (map string->number))))))

(struct $pos (x y))

(define (adjacent-poss pos)
  (match-define ($pos x y) pos)
  (list ($pos (sub1 x) y)
        ($pos (add1 x) y)
        ($pos x (sub1 y))
        ($pos x (add1 y))))

(define (on-map? heightmap pos)
  (match-define ($pos x y) pos)
  (cond
    [(or (negative? x) (negative? y)) #f]
    [(or (> x (sub1 (length (first heightmap))))
         (> y (sub1 (length heightmap)))) #f]
    [else #t]))

(define (adjacent-poss-on-map heightmap pos)
  (~>> (adjacent-poss pos)
       (filter (curry on-map? heightmap))))

(define (pos->value heightmap pos)
  (~> heightmap (list-ref ($pos-y pos)) (list-ref ($pos-x pos))))

(define (find-lowest heightmap)
  (for/fold ([acc '()])
            ([(vs y) (in-indexed heightmap)])
    (for/fold ([acc acc])
              ([(v x) (in-indexed vs)])
      (define is-smallest
        (~>> ($pos x y)
             (adjacent-poss-on-map heightmap)
             (map (curry pos->value heightmap))
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