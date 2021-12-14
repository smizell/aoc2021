#lang racket

(require rackunit
         threading)

(define (part1 filename)
  (define paper (load-paper filename))
  (~>> paper $paper-dots (fold-dots (first ($paper-folds paper))) set-count))

(define (part2 filename)
  (match-define ($paper ds fs) (load-paper filename))
  (~> (foldl fold-dots ds fs) plot-and-display))

(module+ test
  (check-eq? (part1 "../inputs/day13-example.txt") 17))

(struct $paper (dots folds) #:transparent)
(struct $dot (x y) #:transparent)
(struct $fold (axis num) #:transparent)

(define (load-paper filename)
  (match-define (list ds fs) (~> filename file->string (string-split "\n\n")))
  (define ds* (~>> ds
                   (string-split _ "\n")
                   (map (λ~>> (string-split _ ",")
                              (map string->number)
                              (apply $dot)))
                   list->set))
  (define fs* (~>> fs
                   (string-split _ "\n")
                   (map (match-lambda
                          [(pregexp #px"fold along (\\w)=(\\d+)"
                                    (list _ axis num))
                           ($fold (string->symbol axis) (string->number num))]))))
  ($paper ds* fs*))

(define (fold-dots fold dots)
  (match-define ($fold axis num) fold)
  (for/set ([d (in-set dots)])
    (match-define ($dot x y) d)
    (match axis
      ['y (cond
           [(< y num) d]
           [else (struct-copy $dot d [y (- num (- y num))])])]
      ['x (cond
           [(< x num) d]
           [else (struct-copy $dot d [x (- num (- x num))])])])))

(define (get-dimensions dots)
  (for/fold ([max-x 0]
             [max-y 0]
             #:result (list max-x max-y))
            ([d (in-set dots)])
    (values (if (> ($dot-x d) max-x) ($dot-x d) max-x)
            (if (> ($dot-y d) max-y) ($dot-y d) max-y))))

(define (plot dots)
  (match-define (list max-x max-y) (get-dimensions dots))
  (for/list ([y (in-inclusive-range 0 max-y)])
    (for/list ([x (in-inclusive-range 0 max-x)])
      (cond
        [(set-member? dots ($dot x y)) "#"]
        [else "."]))))

(define (plot-and-display dots)
  (~>> dots
       plot
       (map (λ~> (string-join "")))
       (string-join _ "\n")
       displayln))