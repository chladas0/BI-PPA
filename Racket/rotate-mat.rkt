#lang racket

; returns length of the list
(define (ppa-len lst)
  (define (lenhelper lst cnt)
    (cond
      [(empty? lst) cnt]
      [#t (lenhelper (cdr lst) (+ cnt 1))]))
  (lenhelper lst 0))

; takes first n elements from list
(define (ppa-take lst n)
  (cond
    [(or (empty? lst) (= n 0)) '()]
    [#t (cons (car lst) (ppa-take (cdr lst) (- n 1)))]))

;drops first n elements from the list
(define (ppa-drop lst n)
  (cond
    [(or (empty? lst) (= n 0)) lst]
    [#t (ppa-drop (cdr lst) (- n 1) )]))

; appends list1 to list 2
(define (ppa-append lst1 lst2)
  (cond
    [(empty? lst1) lst2]
    [#t (cons (car lst1) (ppa-append (cdr lst1) lst2))]))

; rotates row by n times to the right
(define (rotateRow lst n)
  (define len (ppa-len lst))
  (ppa-append (ppa-drop lst (- len (modulo n len)))
          (ppa-take lst (- len (modulo n len)))))

;return nth element of the list
(define (ppa-nth lst n)
  (cond
    [(empty? lst) 42]
    [(= n 0) (car lst)]
    [#t (ppa-nth (cdr lst) (- n 1))]))

;function to get nth column from matrix
(define (ppa-nth-column matrix n)
  (cond
    [(empty? matrix) '()]
    [#t (map ( lambda (row) (ppa-nth row n)) matrix)]))

;function to replace n-th element in the list
(define (ppa-nth-replace lst n val)
  (cond
    [(or (empty? lst) (< n 0)) lst]
    [(= n 0) (cons val (cdr lst))]
    [#t (cons (car lst) (ppa-nth-replace (cdr lst) (- n 1) val))]))

;function to generate numbers from st to en
(define (ppa-range st en)
  (cond
    [(= st en) '()]
    [#t (cons st (ppa-range (+ st 1) en))]))

; function to replace n-th column with given list
(define (ppa-nth-replace-column matrix n col size)
  (map (lambda (row i) (ppa-nth-replace row n (ppa-nth col i))) matrix (ppa-range 0 size)))

;function to rotate row/col of matrix
(define (rotate-helper matrix command)
  (define idx (ppa-nth command 1))
  (define n   (ppa-nth command 2))
  (define isRow (= (ppa-nth command 0) 0))

  (if isRow
      (ppa-nth-replace matrix idx (rotateRow (ppa-nth matrix idx) n))
      (ppa-nth-replace-column matrix idx (rotateRow (ppa-nth-column matrix idx) n) (ppa-len matrix))))


(define (rotate-mat matrix commands)
  (cond
    [(empty? commands) matrix]
    [#t (rotate-mat (rotate-helper matrix (car commands)) (cdr commands))]))