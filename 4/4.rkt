#lang racket
(require racket/trace)
(define in (open-input-file "sample.txt"))

(define readInput
  (for/list ([line (in-lines in)])
    ; (define cmd (string-split line))
    line)
  )
(close-input-port in)

;stolen from web. Cool function https://stackoverflow.com/a/30776501
;Here is an explanation of apply https://stackoverflow.com/a/27504163
(define (transpose matrix)
  (apply map list matrix))


(define (bingoCheck ogBoard board draws draw)
  ; (displayln "Board")
  ; (displayln board)
   (if (< (length (set->list draws)) 5) 0
      (let ([bingoRow 
        (for/fold ([acc '()])
          ([row board] #:unless (= (length acc) 5))
          (let ([intersection (set->list (set-intersect row draws))]) 
            (if (= (length intersection) 5) intersection '())))])
      (if (eq? bingoRow '()) 0 (countBoard ogBoard bingoRow draws draw))
   ))
)

(define (countBoard ogBoard bingoRow draws lastDraw)  
  (define winnerRowSum (apply + bingoRow)) 
  (define boardSum 
      (for/sum ([row ogBoard])
        (for/sum ([i row] #:when (not (set-member? draws i))) i))
  )
  (- boardSum winnerRowSum)
)
 

; (p1 smallInput)
(define (addToBoard board line)
  (define ns (filter number? (map string->number (regexp-split #px"\\s{1,2}" line))))
  ; (displayln ns)
  (match board 
  [ '() (cons (cons ns '()) '()) ] 
  [ (cons h t)
      #:when (= (length h) 5)
      (cons (cons ns '()) board) ]
  [ (cons h t) (cons (cons ns h) t)]
  )
)

(define (readBoards lines)
  (define (inner acc rows)
    
    (match rows
    [(cons "" tail) (inner acc tail)]
    [(cons row tail) (inner (addToBoard acc row) tail)]
    ['() acc])
  )
  (inner '() lines)
)

(define (diagonals square)
    (define (inner foldfunc square)
      (car (foldfunc (lambda (row acc)
      (let ([index (cdr acc)] [dia (car acc)])
        ; (displayln index)
        ; (displayln row)
        ; (displayln (cons (list-ref row index) dia))
        (cons (cons (list-ref row index) dia) (add1 index))
      ))
      (cons '() 0)
      square
  ))
    )
    (list (inner foldl square) (inner foldr square))
  )
; (diagonals testBoard)

(define (flatSquare square)
  (define diags (diagonals square))
  (map list->set (append square (transpose square) diags)))

(define smallInput 
  '("12,32,1,2,5,3"
    ""
    "22 13 17 11  0"
    " 8  2 23  4 24"
    "21  9 14 16  7"
    " 6 10  3 18  5"
    " 1 12 20 15 19"
    ; ""
    ; "22 13 17 11  0"
    ; " 8  2 23  4 24"
    ; "21  9 14 16  7"
    ; " 6 10  3 18  5"
    ; " 1 12 20 15 19"
     )
)


(define testBoard 
  '((22 13 17 11 0)
    (8 2 23 4 24)
    (21 9 14 16 7)
    (6 10 3 18 5)
    (1 12 20 15 19))
)

(define (solver1 draws boards)
  (displayln "Solvie time")
  (define fixedBoards (for/list ([board boards]) (cons (flatSquare board) board)))
  (define res (for/fold
    ([acc (cons '() 0)])
    ([draw draws] [board fixedBoards] 
      #:unless (> (cdr acc) 0))
    (cons (cons draw (car acc)) (bingoCheck (cdr board) (car board) (list->set (cons draw (car acc))) draw))))
  (cdr res)
)

(define (p1 input)
  (define draws 
    (map string->number (string-split (car input) #rx","))
    )
  (define boards (readBoards (cdr (cdr input))))
  (solver1 draws boards)
)


;tsolver
(p1 readInput)