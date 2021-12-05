#lang racket
(require racket/trace)
(define in (open-input-file "4/input.txt"))

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



(define (p1 input)
  (define draws 
    (map string->number (string-split (car input) #rx","))
    )
  (define boards (readBoards (cdr (cdr input))))
  (cons draws (map flatSquare boards)) 
)
(p1 smallInput)
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
  (display "diag: ")(displayln diags)
  (append square (transpose square) diags)
)

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