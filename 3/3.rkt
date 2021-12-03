#lang racket

(define in (open-input-file "3/input.txt"))

(define readInput
  (for/list ([line (in-lines in)])
    (define cmd (string-split line))
    cmd)
  )
(close-input-port in)

;stolen from web. Cool function https://stackoverflow.com/a/30776501
;Here is an explanation of apply https://stackoverflow.com/a/27504163
(define (transpose matrix)
  (apply map list matrix))


(define (toDecimal binary)
    (cdr 
        (for/foldr
          ([acc (cons 0 0)])
          ([i binary])
          (let ([n (car acc)] [count (cdr acc)])
          (cons (add1 n) (+ count (* (expt 2 n) i)))))
))

(define (to2dIntList input) (for/list ([row input])
  (let ([chars (string->list (car row))]) 
    (for/list ([c chars])
      (if (eq? c #\0) 0 1)))))

(define (gamma 2dlist half) (for/list ([row (transpose 2dlist)])
  (if (> (length (filter positive? row)) half) 1 0)))

(define (epsilon 2dlist half) (map (lambda (x) (modulo (+ x 1) 2)) (gamma 2dlist half)))  

(define (part1Solver input)
  (define half (/ (length input) 2))
  (define 2dlist (to2dIntList input))
  (* (toDecimal (epsilon 2dlist half)) (toDecimal (gamma 2dlist half)))
) 

(define (part2Solver input)
  input
)