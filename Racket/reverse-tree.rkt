#lang racket

(define (ppa-list . params)
  params)

(define (reverse-tree tree)
  (cond
    ; base case : tree is empty
    [(empty?  tree) tree]
    
    ; swap left and right subtree, recursively swap their left, right subtree ... 
    [ (let ((value (car tree  ))
           ( left  (cadr tree ))
           ( right (caddr tree)))
       (ppa-list value (reverse-tree right) (reverse-tree left)))]))
   