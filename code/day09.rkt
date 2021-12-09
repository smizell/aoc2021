#lang racket

(require rackunit
         threading)

(define (load-heightmap filename)
  (~>> filename
       file->string
       (string-split _ "\n")
       (map (λ (l) (~>> l string->list (map string) (map string->number))))))

(struct $pos (x y) #:transparent)

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
  (for*/fold ([acc '()])
             ([(vs y) (in-indexed heightmap)]
              [(v x) (in-indexed vs)])
    (define pos ($pos x y))
    (define is-smallest
      (~>> pos
           (adjacent-poss-on-map heightmap)
           (map (curry pos->value heightmap))
           (andmap (curry < v))))
    (if is-smallest (cons pos acc) acc)))

(define (part1 filename)
  (define heightmap (~>> filename load-heightmap))
  (~>> heightmap
       find-lowest
       (map (curry pos->value heightmap))
       (map add1)
       (apply +)))

(module+ test
  (check-eq? (part1 "../inputs/day09.txt") 458))

(define (in-basin? heightmap pos)
  (< (pos->value heightmap pos) 9))

(define (find-basin heightmap pos [basin '()])
  (define ps (adjacent-poss-on-map heightmap pos))
  (define bps (filter (curry in-basin? heightmap) ps))
  (define unexplored (set->list (set-subtract (list->set bps) (list->set basin))))
  (define new-basin
    (cond
      [(in-basin? heightmap pos) (cons pos (append unexplored basin))]
      [else (append unexplored basin)]))
  (for/fold ([acc new-basin])
            ([p (in-list unexplored)])
    (find-basin heightmap p acc)))

; My code produced duplicates so I convert to a set and back.
; It's ugly, but works so I'm leaving it.
(define (find-basin* heightmap pos)
  (set->list (list->set (find-basin heightmap pos))))

(define (part2 filename)
  (define heightmap (~>> filename load-heightmap))
  (~>> heightmap
       find-lowest
       (map (curry find-basin* heightmap))
       (map (λ (b) (map (curry pos->value heightmap) b)))
       (map length)
       (sort _ >)
       (take _ 3)
       (apply *)))

(module+ test
  (check-eq? (part2 "../inputs/day09.txt") 1391940))