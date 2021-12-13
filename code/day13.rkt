#lang racket

(require threading)

(struct $paper (dots folds) #:transparent)
(struct $dot (x y) #:transparent)
(struct $fold (axis num) #:transparent)

(define (load-paper filename)
  (match-define (list dots folds) (~> filename file->string (string-split "\n\n")))
  ($paper (~>> dots
               (string-split _ "\n")
               (map (λ (ns) (~>> ns (string-split _ ",") (map string->number))))
               (map (curry apply $dot)))
          (~>> folds
               (string-split _ "\n")
               (map (λ (i)
                      (match-define (list _ axis num) (regexp-match #px"fold along (\\w)=(\\d+)" i))
                      ($fold (string->symbol axis) (string->number num)))))))

(define (fold-dots dots fold)
  (match-define ($fold axis num) fold)
  (for/set ([d (in-list dots)])
    (match axis
      ['y (cond
           [(< ($dot-y d) num) d]
           [else (struct-copy $dot d [y (- num (- ($dot-y d) num))])])]
      ['x (cond
           [(< ($dot-x d) num) d]
           [else (struct-copy $dot d [x (- num (- ($dot-x d) num))])])])))

(define (part1 filename)
  (define paper (load-paper filename))
  (~> paper $paper-dots (fold-dots (first ($paper-folds paper))) set-count))

(define (get-dimensions dots)
  (for/fold ([max-x 0]
             [max-y 0]
             #:result (list max-x max-y))
            ([d (in-list dots)])
    (values (if (> ($dot-x d) max-x) ($dot-x d) max-x)
            (if (> ($dot-y d) max-y) ($dot-y d) max-y))))

(define (plot dots dimensions)
  (match-define (list max-x max-y) dimensions)
  (for/list ([x (in-inclusive-range 0 max-x)])
    (for/list ([y (in-inclusive-range 0 max-y)])
      (cond
        [(set-member? dots ($dot x y)) "#"]
        [else "."]))))

; Prints it sideways and backwards
; Just leaving for now
(define (part2 filename)
  (match-define ($paper ds fs) (load-paper filename))
  (define ds*
    (for/fold ([ds ds] #:result (list->set ds))
              ([f (in-list fs)])
      (set->list (fold-dots ds f))))
  (define dimensions (get-dimensions (set->list ds*)))
  (define p (plot ds* dimensions))
  (for ([l (in-list (map (λ (l) (~> l (string-join ""))) p))])
    (println l)))