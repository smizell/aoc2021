#lang racket

(require threading)

(define (load-info filename)
  (~>> filename
       file->string
       (string-split _ "\n")
       (map (λ (l) (string-split l " | ")))
       (map (λ (l) (map (λ (v)
                          (~>> v
                               string-split
                               (map (λ (s) (~>> s string->list (map string))))))
                        l)))))

(define (part1 filename)
  (~>> filename
       load-info
       (map second)
       (map (λ (v) (map length v)))
       (map (λ (v) (filter (λ (l) (or (= l 2) (= l 3) (= l 4) (= l 7))) v)))
       (map length)
       (apply +)))

(define (determine-digits input)
  ; Pull out the unique into a hash for lookup later
  (define-values (unique remaining)
    (for/fold ([unique (hash)]
               [remaining '()])
              ([i (in-list input)])
      (match (length i)
        [2 (values (hash-set unique 1 (list->set i)) remaining)]
        [3 (values (hash-set unique 7 (list->set i)) remaining)]
        [4 (values (hash-set unique 4 (list->set i)) remaining)]
        [7 (values (hash-set unique 8 (list->set i)) remaining)]
        [_ (values unique (cons (list->set i) remaining))])))
  (define found
    (for/fold ([acc (hash)])
              ([r (in-list remaining)])
      (define rl (sort (set->list r) string<=?))
      (match (length (set->list r))
        [5 (cond
             ; 3 is 5 long and includes 1
             [(subset? (hash-ref unique 1) r) (hash-set acc rl 3)]
             ; 5 is 5 long and includes the diff of 1 and 4
             [(subset? (set-subtract (hash-ref unique 4) (hash-ref unique 1)) r) (hash-set acc rl 5)]
             ; 2 is 5 long and doesn't match the above
             [else (hash-set acc rl 2)])]
        [6 (cond
             ; 9 is 6 long and includes 1
             [(subset? (hash-ref unique 1) r) (hash-set acc rl 9)]
             ; 6 is 6 long and doesn't include 1
             [else (hash-set acc rl 6)])])))
  (for/fold ([acc found])
            ([(n s) (in-hash unique)])
    (hash-set acc (sort (set->list s) string<=?) n)))

(define (part2 filename)
  (~>> filename
       load-info
       (map (match-lambda
              [(list ds os)
               (list (determine-digits ds)
                     (map (λ (o) (sort o string<=?)) os))]))
       (map (match-lambda
              [(list vs o)
               (map (curry hash-ref vs) o)]))
       (map (λ (n)
              (~>> n
                   (map number->string)
                   (string-join _ "")
                   string->number)))
       (apply +)))