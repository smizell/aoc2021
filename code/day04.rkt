#lang racket

(require rackunit
         threading)

(struct $game (nums boards) #:transparent)
(struct $cell (num marked?) #:transparent)

(define (load-game filename)
  (define file (file->string filename))
  (define content (string-split file "\n\n"))
  (define nums (~> content first (string-split ",") (map string->number _)))
  (define boards (map parse-board (rest content)))
  ($game nums boards))

(define (parse-board content)
  (~> content
      (string-split "\n")
      (map string-split _)
      (map (λ (r)
             (map (λ (n)
                    (~> n string->number ($cell #f))) r)) _)))

(define (cells-complete? cs)
  (for/and ([c (in-list cs)])
    ($cell-marked? c)))

(module+ test
  (define row-not-complete (list ($cell 14 #f) ($cell 21 #f) ($cell 17 #f) ($cell 24 #f) ($cell 4 #f)))
  (define row-complete (map (λ (c) (struct-copy $cell c [marked? #t])) row-not-complete))
  (check-false (cells-complete? row-not-complete))
  (check-true (cells-complete? row-complete)))

(define (mark-cells css n)
  (map (λ (r)
         (map (λ (c)
                (if (= ($cell-num c) n)
                    (struct-copy $cell c [marked? #t])
                    c))
              r))
       css))

(module+ test
  (define marked (mark-cells (list row-not-complete) 14))
  (check-true (~> marked first first $cell-marked?)))

(define (transpose nss)
  (apply map list nss))

(define (board-won? rs)
  (or (for/or ([r (in-list rs)])
        (cells-complete? r))
      (for/or ([r (in-list (transpose rs))])
        (cells-complete? r))))

(module+ test
  (check-false (board-won? (list row-not-complete)))
  (check-true (board-won? (list row-complete)))
  ; Check columns
  (check-true (board-won? (list (list ($cell 4 #t) ($cell 5 #f))
                                (list ($cell 10 #t) ($cell 13 #f))))))

(define (play-game game)
  (let loop ([ns ($game-nums game)]
             [bs ($game-boards game)])
    (define n (first ns))
    (define rns (rest ns))
    (define marked-bs (map (λ (b) (mark-cells b n)) bs))
    (define winner (for/or ([b (in-list marked-bs)])
                     (if (board-won? b) b #f)))
    (cond
      [winner (values ns winner)]
      [(empty? rns) (values ns #f)] ; No winners
      [else (loop (rest ns) marked-bs)])))

(define (play-game-file filename)
  (play-game (load-game filename)))

(define (unmarked-cells css)
  (for/fold ([acc '()])
            ([cs (in-list css)])
    (append acc (filter (λ (c) (false? ($cell-marked? c))) cs))))

(module+ test
  (check-equal? (unmarked-cells (list (list ($cell 4 #f) ($cell 10 #t))))
                (list ($cell 4 #f))))

(define (calculate ns board)
  (* (apply + (~>> board unmarked-cells (map $cell-num)))
     (first ns)))

(define (part1 filename)
  (call-with-values (λ () (play-game-file filename)) calculate))

(define (part2 filename)
  (define game (load-game filename))
  ; Play each board separately and see how many nums are left.
  ; The one with the least left is the last board.
  (~>> game
       $game-boards
       (map (λ (b) ($game ($game-nums game) (list b))))
       (map (λ (g) (call-with-values (λ () (play-game g)) list)))
       (filter (λ (r) (second r))) ; remove boards that never win
       (sort _ < #:key (λ (r) (length (first r))))
       first
       (apply calculate)))
