#lang racket

(define (getCmb lst n cnt)
  (cond
    ; found sum equal to n
    [(= cnt n) 1]
    ; the list is empty or we exceeded n
    [(or (empty? lst) (> cnt n)) 0]
    ; get combinations with current number involved and without
    [#t ( + (getCmb (cdr lst) n (+ cnt (car lst)))
            (getCmb (cdr lst) n cnt))]))

(define (comb lst n)
  (getCmb lst n 0))