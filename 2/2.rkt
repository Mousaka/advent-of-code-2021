#lang racket

(define in (open-input-file "2/input.txt"))

(define readInput
  (for/list ([line (in-lines in)])
    (define cmd (string-split line))
    cmd)
  )
(close-input-port in)

(define (part1Solver input)
  (define fixedInput (for/list ([i input]) (cons (string->symbol (first i)) (string->number (first (rest i))))))
  (define counts
    (foldl
     (lambda (cmd acc) 
       (let ([depthAcc (car acc)]
             [horizontalAcc (cdr acc)]
             [direction (car cmd)]
             [units (cdr cmd)]) 
         (cond
           [(eq? direction 'down ) (cons (+ depthAcc units) horizontalAcc)]
           [(eq? direction 'up ) (cons (- depthAcc units) horizontalAcc)]
           [(eq? direction 'forward ) (cons depthAcc (+ horizontalAcc units))]
           )))
     (cons 0 0)
     fixedInput)
    )
  (* (car counts) (cdr counts)))


(define (part2Solver input)
  (define fixedInput (for/list ([i input]) (cons (string->symbol (first i)) (string->number (first (rest i))))))
  (define counts
    (foldl
     (lambda (cmd acc) 
       (let ([depthAcc (car acc)]
             [horizontalAcc (car (cdr acc))]
             [direction (car cmd)]
             [units (cdr cmd)]
             [aimAcc (cdr (cdr acc))]) 
         (cond
           [(eq? direction 'down ) (cons depthAcc (cons horizontalAcc (+ aimAcc units)))]
           [(eq? direction 'up ) (cons depthAcc (cons horizontalAcc (- aimAcc units)))]
           [(eq? direction 'forward ) (cons (+ depthAcc (* aimAcc units)) (cons (+ horizontalAcc units) aimAcc))]
           )))
     (cons 0 (cons 0 0))
     fixedInput)
    )
  (* (car counts) (car (cdr counts))))