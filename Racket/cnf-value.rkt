#lang racket

(define (getFirst pair)
  (car pair))

(define (getSecond pair)
  (cadr pair))

;find x key and return its value
(define (getValue lst x)
  (cond
    [(= (getFirst (car lst)) x) (getSecond (car lst))]
    [#t (getValue (cdr lst) x)]))

; takes array and makes or between the elements
(define (evaluateOr lst values)
  (cond
    [(empty? lst) #f]
    [#t (or (getValue values (car lst)) (evaluateOr (cdr lst) values))]))

; use evaluateOr on each operand and use and as operator
(define (cnf-value form values)
  (cond
    [(empty? form) #t]
    [#t (and (evaluateOr (car form) values) (cnf-value (cdr form) values))]))  