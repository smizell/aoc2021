#lang racket

(require threading)

(define (load-map filename)
  (~>> filename
       file->string
       (string-split _ "\n")
       (map (λ (l) (~>> l string->list (map string) (map string->number))))))

(define (adjacent-pos* x y)
  (cartesian-product (list (sub1 x) x (add1 x))
                     (list (sub1 y) y (add1 y))))

(define (adjacent-pos x y)
  (list (list (sub1 x) y)
        (list (add1 x) y)
        (list x (sub1 y))
        (list x (add1 y))))

(define (find-lowest heightmap)
  (for/fold ([acc '()])
            ([(vs y) (in-indexed heightmap)])
    (for/fold ([acc acc])
              ([(v x) (in-indexed vs)])
      (define is-smallest
        (~>> (adjacent-pos x y)
             (filter (match-lambda
                       [(list ax ay)
                        (cond
                          [(or (negative? ax) (negative? ay)) #f]
                          [(or (> ax (sub1 (length vs))) (> ay (sub1 (length heightmap)))) #f]
                          [else #t])]))
             (map (match-lambda
                    [(list ax ay) (~> heightmap (list-ref ay) (list-ref ax))]))
             (andmap (curry < v))))
      (if is-smallest (cons v acc) acc))))

(define (part1 filename)
  (~>> filename
       load-map
       find-lowest
       (map add1)
       (apply +)))