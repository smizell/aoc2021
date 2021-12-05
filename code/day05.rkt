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

(define (build-diagram x y)
  (build-list (add1 y) (λ (n) (build-list (add1 x) (λ (n) 0)))))

(define (add-coordinate diagram coor)
  (match-define (list x y) coor)
  (define n (~> diagram (list-ref y) (list-ref x) add1))
  (~> diagram  (list-set y (~> diagram (list-ref y) (list-set x n)))))

(define (line->coors line)
  (match line
    [`((,x ,y1) (,x ,y2))
     (define y (if (< y1 y2) (range y1 (add1 y2)) (range y2 (add1 y1))))
     (cartesian-product (list x) y)]
    [`((,x1 ,y) (,x2 ,y))
     (define x (if (< x1 x2) (range x1 (add1 x2)) (range x2 (add1 x1))))
     (cartesian-product x (list y))]
    [`((,x1 ,y1) (,x2 ,y2))
     ; we have to reverse the ranges because range only works with ascending nums
     (define x-list (if (< x1 x2) (range x1 (add1 x2)) (reverse (range x2 (add1 x1)))))
     (define y-list (if (< y1 y2) (range y1 (add1 y2)) (reverse (range y2 (add1 y1)))))
     (for/list ([x (in-list x-list)]
                [y (in-list y-list)])
       (list x y))]))

(define (diagonal? line)
  (match-define (list (list x1 y1) (list x2 y2)) line)
  (not (or (= x1 x2) (= y1 y2))))

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

(define (part1 filename)
  (define lines (~>> filename load-lines (filter-not diagonal?)))
  (define diagram (for/fold ([d (apply build-diagram (diagram-dimensions lines))])
                            ([l (in-list lines)])
                    (plot-line d l)))
  (for/fold ([acc 0])
            ([r (in-list diagram)])
    (+ acc (count (λ (n) (> n 1)) r))))

(define (part2 filename)
  (define lines (~>> filename load-lines))
  (define diagram (for/fold ([d (apply build-diagram (diagram-dimensions lines))])
                            ([l (in-list lines)])
                    (plot-line d l)))
  (for/fold ([acc 0])
            ([r (in-list diagram)])
    (+ acc (count (λ (n) (> n 1)) r))))
