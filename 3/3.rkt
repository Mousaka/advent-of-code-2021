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

(define (patternFilter pos n)
  (lambda (row) 
    (= n (list-ref row pos))
  )
)

(define (gamma2 2dlist) 
  (define half (/ (length 2dlist) 2))
  (for/list ([row (transpose 2dlist)])
  (if (>= (length (filter positive? row)) half) 1 0)))

(define (epsilon2 2dlist) 
  (define half (/ (length 2dlist) 2))
  (for/list ([row (transpose 2dlist)])
  (if (>= (length (filter positive? row)) half) 0 1)))

(define (part2Solver input)

  (define (inner mfunc bitPos alist) 
    (define ga (mfunc alist))
    (if (or (= 1 (length alist)) (= bitPos (length ga)))
      alist
      (inner mfunc (add1 bitPos) (filter (patternFilter bitPos (list-ref ga bitPos)) alist))
    ))

   (* (toDecimal (first (inner epsilon2 0 (to2dIntList input))))
      (toDecimal (first (inner gamma2 0 (to2dIntList input)))))
)

(part1Solver readInput)
(part2Solver readInput)