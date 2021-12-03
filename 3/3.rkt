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


(define (part1Solver input)
  (define indeces (build-list (length input) values))
  (define 2dIntList (for/list ([row input])
    (let ([chars (string->list (car row))]) 
      (for/list ([c chars])
        (if (eq? c #\0) 0 1)))))
  (define gamma (for/list ([row (transpose 2dIntList)])
    (if (> (length (filter positive? row)) (/ (length input) 2)) 1 0)))
  (define epsilon (map (lambda (x) (modulo (+ x 1) 2)) gamma))  

  (* (toDecimal epsilon) (toDecimal gamma))
) 
