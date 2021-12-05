#lang racket

(require threading)

(define (load-lines filename)
  (define lines (file->lines filename))
  (~>> (file->lines filename)
       (map (λ (l)
              (match (regexp-match #px"(\\d*),(\\d*) -> (\\d*),(\\d*)" l)
                [(list _ x1 y1 x2 y2)
                 (list (list (string->number x1) (string->number y1))
                       (list (string->number x2) (string->number y2)))])))))

(define (line->coors line)
  (match line
    [`((,x ,y1) (,x ,y2))
     (define ys (if (< y1 y2) (range y1 (add1 y2)) (range y2 (add1 y1))))
     (cartesian-product (list x) ys)]
    [`((,x1 ,y) (,x2 ,y))
     (define xs (if (< x1 x2) (range x1 (add1 x2)) (range x2 (add1 x1))))
     (cartesian-product xs (list y))]
    [`((,x1 ,y1) (,x2 ,y2))
     ; we have to reverse the ranges because range only works with ascending nums
     (define xs (if (< x1 x2) (range x1 (add1 x2)) (reverse (range x2 (add1 x1)))))
     (define ys (if (< y1 y2) (range y1 (add1 y2)) (reverse (range y2 (add1 y1)))))
     (for/list ([x (in-list xs)]
                [y (in-list ys)])
       (list x y))]))

(define (diagonal? line)
  (match-define (list (list x1 y1) (list x2 y2)) line)
  (not (or (= x1 x2) (= y1 y2))))

(define (calculate* lines)
  (define cs (~>> lines (map line->coors) (apply append)))
  (define results
    (for/fold ([acc (hash)])
              ([c (in-list cs)])
      (hash-set acc c (add1 (hash-ref acc c 0)))))
  (~>> results hash-values (filter (λ (v) (> v 1))) length))

(define (part1* filename)
  (~>> filename load-lines (filter-not diagonal?) calculate*))

(define (part2* filename)
  (~>> filename load-lines calculate*))


; -------
; This code below works, but I should have thought about the problem more
; before jumping into the solution. The one above is much simpler and faster.

(define (build-diagram x y)
  (build-list (add1 y) (λ (n) (build-list (add1 x) (λ (n) 0)))))

(define (add-coordinate diagram coor)
  (match-define (list x y) coor)
  (define n (~> diagram (list-ref y) (list-ref x) add1))
  (~> diagram  (list-set y (~> diagram (list-ref y) (list-set x n)))))

(define (plot-line diagram line)
  (define coors (line->coors line))
  (for/fold ([d diagram])
            ([c (in-list coors)])
    (add-coordinate d c)))

(define (diagram-dimensions lines)
  (for/fold ([x 0]
             [y 0]
             #:result (list x y))
            ([l (in-list lines)])
    (match-define (list (list x1 y1) (list x2 y2)) l)
    (define-values (max-x max-y) (values (max x1 x2) (max y1 y2)))
    (values (if (> max-x x) max-x x)
            (if (> max-y y) max-y y))))

(define (calculate lines)
  (define diagram (for/fold ([d (apply build-diagram (diagram-dimensions lines))])
                            ([l (in-list lines)])
                    (plot-line d l)))
  (for/fold ([acc 0])
            ([r (in-list diagram)])
    (+ acc (count (λ (n) (> n 1)) r))))

(define (part1 filename)
  (~>> filename load-lines (filter-not diagonal?) calculate))

(define (part2 filename)
  (~>> filename load-lines calculate))
