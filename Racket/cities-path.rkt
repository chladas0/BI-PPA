#lang racket

(define (get-first pair)
  (car pair))

(define (get-second pair)
  (cadr pair)) 

;custom list lib func
(define (ppa-list . params)
  params)

;custom ormap
(define (ppa-ormap f lst)
  (cond
    [(empty? lst) #f]
    [(f (get-first lst)) #t]
    [#t (ppa-ormap f (cdr lst))]))

;function to insert into the map
(define (map-insert tree key value)
  
  (cond
    ; tree is empty, we found place for the key
    [(empty? tree) ( ppa-list (ppa-list key (ppa-list value)) '() '() )]

    ;insert into left subtree
    [(< key (get-first (car tree)))
       (ppa-list (car tree) (map-insert (cadr tree) key value) (caddr tree))]

    ;insert into right subtree
    [(> key (get-first (car tree)))
     (ppa-list (car tree) (cadr tree) (map-insert (caddr tree) key value))]

    [#t
      (ppa-list (ppa-list key (cons value (get-second (car tree))) ) (cadr tree) (caddr tree))]))

;function that takes key and returns its value
(define (map-at tree key)
  (cond
    ;not found
    [(empty? tree) '()]

    ;go left
    [(< key (get-first (car tree)))
        (map-at (cadr tree) key)]

    ;go right
    [(> key (get-first (car tree)))
        (map-at (caddr tree) key)]

    ;found
    [#t (get-second (car tree)) ] ))
    
;takes key and return #t or #f if it contains the key
(define (map-contains tree key)
  (cond
    ;not found
    [(empty? tree) #f]

    ;go left
    [(< key (get-first (car tree)))
        (map-contains (cadr tree) key)]

    ;go right
    [(> key (get-first (car tree)))
        (map-contains (caddr tree) key)]

    ;found
    [#t #t]))

;build graph - iterate through paths and call map insert from to
(define (build-graph paths)
  (foldl (lambda (path g)
           (map-insert g (get-first path) (get-second path))) '() paths))

(define (cities-path? paths from to )
  (dfs from '() (build-graph paths) to))

(define (dfs cur visited g to)
  ;loop check
  (if(map-contains visited cur) #f
     ;true if (current == to) or there is path in his neighbours
     (or (= cur to) (ppa-ormap (lambda (neig)
           (dfs neig (map-insert visited cur #t) g to))
               (map-at g cur)))))