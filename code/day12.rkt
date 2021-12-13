#lang racket

(require threading
         rackunit)

(struct $state (caves visited paths) #:transparent)

(define (load-caves filename)
  (~>> filename
       file->list
       (map (λ (l) (map string->symbol (string-split (symbol->string l) "-"))))))

(define (edges-for caves cave)
  (~>> caves
       (filter (curry member cave))
       (map (λ (c) (filter-not (λ (v) (eq? v cave)) c)))
       (map first)))

(define (start? cave)
  (eq? 'start cave))

(define (end? cave)
  (eq? 'end cave))

(define (start-or-end? cave)
  (or (start? cave) (end? cave)))

(define (small? caves)
  (and (eq? (~> caves symbol->string string-downcase string->symbol) caves)
       (not (start-or-end? caves))))

(define (big? caves)
  (not (small? caves)))

(define (find-next state cave)
  (match-define ($state caves visited _) state)
  (define vs (edges-for caves cave))
  (filter (λ (v)
            (cond
              [(end? v) #t]
              [(start? v) #f]
              [(and (small? v) (not (member v visited))) #t]
              [(big? v) #t]
              [else #f])) vs))

(define (traverser state next-fn)
  (match-define ($state _ (list v vs ...) paths) state)
  (cond
    [(end? v) (struct-copy $state state [paths (cons ($state-visited state) paths)])]
    [else (for/fold ([new-state state])
                    ([nv (next-fn state v)])
            (define visited* (cons nv ($state-visited state)))
            (traverser (struct-copy $state new-state [visited visited*])
                       next-fn))]))

(define (traverse-caves caves [next-fn find-next])
  (~> (traverser ($state caves '(start) '()) next-fn) $state-paths))

(define (part1 filename)
  (~> filename load-caves traverse-caves length))

(define (visit-count vs)
  (for/fold ([acc (hash)])
            ([v (in-list vs)])
    (hash-update acc v add1 0)))

(define (any-two? vc)
  (let loop ([vs (hash-values vc)])
    (cond
      [(empty? vs) #f]
      [else (cond
              [(>= (car vs) 2) #t]
              [else (loop (cdr vs))])])))

(define (find-next* state cave)
  (match-define ($state caves visited _) state)
  (define vs (edges-for caves cave))
  (define vc (visit-count (filter small? visited)))
  (define has-two? (any-two? vc))
  (filter (λ (v)
            (cond
              [(end? v) #t]
              [(start? v) #f]
              [(small? v) (cond
                            [(and has-two? (= (hash-ref vc v 0) 0)) #t]
                            [(not has-two?) #t]
                            [else #f])]
              [(big? v) #t]
              [else #f])) vs))

(define (part2 filename)
  (~> filename load-caves (traverse-caves find-next*) length))

(module+ test
  (check-eq? (part2 "../inputs/day12-example.txt") 36))