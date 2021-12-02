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
	(define counts (foldl (lambda (cmd acc) (cond
	  							[(eq? (car cmd) 'down ) (cons (+ (car acc) (cdr cmd)) (cdr acc))]
								[(eq? (car cmd) 'up ) (cons (- (car acc) (cdr cmd)) (cdr acc))]
								[(eq? (car cmd) 'forward ) (cons (car acc) (+ (cdr acc) (cdr cmd)))]
								))
								(cons 0 0)
								fixedInput)
								)
	(* (car counts) (cdr counts)))
								
								