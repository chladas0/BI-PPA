#lang racket
(require compatibility/defmacro)
;inc
(define (inc n)
  (+ n 1))

;zero
(define (zero n)
  (= 0 n))

;atom

(define (atom? x)
  (not (or (null? x) (pair? x))))

;(() () ()) check
(define (plusoneifzero n)
   ( if (= n 0) (+ n 1) n))

;lst len
(define (lstLen lst)
  (if (null? lst)
      0
      (if (atom? (car lst))
          (+ 1 (lstLen (cdr lst)))
          ( + (plusoneifzero (lstLen (car lst))) (lstLen (cdr lst))))))

;min
(define (my-min x y)
  (if (< x y) x y))

;max
(define (my-max x y)
  (if (< x y ) y x))

;rec
(define (myfact n)
  (if (= n 0) 1
      (* n (myfact (- n 1) ) )))

;tailrec no check for negative numbers
(define (facttailrec n acc)
  (if (<= n 0)
      acc
      (facttailrec (- n 1) (* acc n))))

(define (facttail n )
  (facttailrec n 1 ))

;sum rec
(define (mysum lst)
  (if (empty? lst)
      0
      (+ (car lst) (mysum (cdr lst)))))

;sum tailrec

(define (mysumRec lst acc)
  (if (null? lst)
      acc
      (mysumRec (cdr lst) (+ acc (car lst)) )))

(define (mysumTail lst)
  (mysumRec lst 1 ))

;nth
(define (nth lst n )
  (if (empty? lst) -1
  (if (= n 0 ) (car lst) (nth (cdr lst) (- n 1)))))

;mymin
(define (minlstrec lst min)
  (if (empty? lst)
      min
      (minlstrec (cdr lst) (my-min min (car lst)) )))

(define (minlst lst)
  (minlstrec lst +inf.0))

;min using fold(proc init lst)

(define (foldmin lst)
  (foldl my-min +inf.0 lst))

;slow fib
(define (myFib n)
  (cond
    [(= n 0) 0]
    [(= n 1) 1]
    [#t (+ (myFib (- n 1)) (myFib (- n 2)))]
    ))

;fast fib
(define (myFibrec n prev cur acc)
  (if (= n 0)
      acc
      (myFibrec (- n 1) cur (+ prev cur) (+ acc cur))))
      
(define (myFib2 n)
  (cond
    [(= n 0) 0]
    [(= n 1) 1]
    [#t (myFibrec (- n 2) 0 1 1)]))

(define (myMember lst e)
  (cond
    [(null? lst) #f]
    [(= (car lst) e ) #t]
    [#t (myMember (cdr lst) e)]))

(define (notMember lst e)
  (if (myMember lst e) #f #t))


(define (myRangeRec n acc)
  (if (= n 0)
      acc
      (myRangeRec (- n 1) (cons n acc))))

(define (myRange n)
  (myRangeRec n null))

(define (myAppend lst e)
  (if (null? lst)
      (cons e null)
      (cons (car lst) (myAppend (cdr lst) e) )))

(define (myAppendList lst1 lst2)
  (if (null? lst1)
      lst2
      (cons (car lst1) (myAppendList (cdr lst1) lst2))))

(define (myInsert e n lst)
  (cond
    [(= n 0) (cons e lst)]
    [(null? lst) lst]
    [#t (cons (car lst) (myInsert e (- n 1) (cdr lst)))]))

(define (myRepeat e n lst)
  (if (= n 0)
      lst
      (myRepeat e (- n 1) (cons e lst) ) ))

(define (myReverse lst acc)
  (if (null? lst)
      acc
      (myReverse (cdr lst) (cons (car lst ) acc))))

;delete first e 
(define (myDelete lst e)
  (if (empty? lst) lst
      
  (if (= e (car lst))
      (cdr lst)
      (cons (car lst) (myDelete  (cdr lst) e)))))

;delete all e 
(define (myDelete2 lst e)
  (if (empty? lst) lst
  (if (= e (car lst))
      (myDelete2 (cdr lst) e)
      (cons (car lst) (myDelete2 (cdr lst) e)))))

;delete last e
(define (myDelete3 lst e)
  (myReverse (myDelete (myReverse lst '())  e) '()))

;myInsert between all

(define (myInsertBetweenAll lst e)
  (cons (car lst) (myInsertBetweenAllRec (cdr lst) e)))

(define (myInsertBetweenAllRec lst e)
  (if (null? lst)
      lst
      (cons e (cons (car lst) (myInsertBetweenAllRec (cdr lst) e)))))

(define (myDup lst e)
  (cond
    [(null? lst) lst]
    [(= (car lst) e)  (cons e (cons e (myDup (cdr lst) e)))] 
    [#t (cons (car lst) (myDup (cdr lst) e))]))

;------------------------------------------------------------------------------

(define (mymemberS lst e)
  (cond
  [(null? lst) #f]
  [(atom? (car lst)) (if (= (car lst) e) #t (mymemberS (cdr lst) e))]
  [#t (or (mymemberS (car lst) e) (mymemberS (cdr lst) e) )]))

;sum including sublists
(define (mysumlst lst)
  (cond
  [(null? lst) 0]
  [(atom? (car lst)) (+ (car lst) (mysumlst (cdr lst)))]
  [#t (+ (mysumlst (car lst)) (mysumlst (cdr lst)) )]))

(define (mymeanS lst)
  (/ (mysumlst lst) (lstLen lst)))

;min including sublists
(define (myminLstRec lst min)
  (cond
    
    [(null? lst) min]
    
    [(atom? (car lst)) (myminLstRec (cdr lst) (my-min min (car lst)) )]
    
    [#t (my-min (myminLstRec (car lst) min) (myminLstRec (cdr lst) min)) ]))

(define (myminLst lst)
  (myminLstRec lst +inf.0))

;flatten
(define (myflattenS lst)
  (cond
    [(empty? lst) lst]
    [(atom? (car lst)) (cons (car lst) (myflattenS (cdr lst)))]
    [#t (myAppendList (myflattenS (car lst)) (myflattenS (cdr lst)))]
  )
)

;------------------------------------------------------------------------
;takes first n elements
(define (take-n lst n)
  (if (or (<= n 0) (null? lst))
      null
      (cons (car lst) (take-n (cdr lst) (- n 1)))))

;drops first n elements
(define (drop-n lst n )
  (if (or (<= n 0) (null? lst))
      lst
      (drop-n (cdr lst) (- n 1))))

; bacha na <= u take a drop jinak to nefunguje s floatama
(define (my-split lst)
  (let ( [half (/ (length lst) 2)])
  (list (take-n lst half) (drop-n lst half))))


(define (myMerge lst1 lst2)
  (cond
    [(null? lst1) lst2]
    [(null? lst2) lst1]
    [(< (car lst1) (car lst2)) (cons (car lst1) (myMerge (cdr lst1) lst2))]
    [#t(cons (car lst2) (myMerge lst1 (cdr lst2)))]
    ))

; bacha na cadr u splitted, cdr da (e)
(define (mergesort lst)
  (cond
    [(or (null? lst) (null? (cdr lst))) lst]  
    [#t (let ([splitted (my-split lst)])
      (myMerge (mergesort (car splitted)) (mergesort (cadr splitted))))]))
;-----------------------------------------------------------------------------

(define (myPrimeRec n i )
  (cond
    [( > (* i i) n) #t]
    [(= (modulo n i ) 0) #f]
    [#t (myPrimeRec n (+ i 1))]))

(define (myPrime n)
  (myPrimeRec n 2))

;pecka magic
(define (my-powerset lst)
  (cond
    ((null? lst)       null)
    ((null? (cdr lst)) (list null (list (car lst))))
    (#t (cartesian (my-powerset (cons (car lst) null)) (my-powerset (cdr lst))))))

(define (cartesian set1 set2)
  (if (null? set1)
      null
      (append (cartesian-helper (car set1) set2) (cartesian (cdr set1) set2))))

(define (cartesian-helper e set)
  (if (null? set)
      null
      (cons (append e (car set)) (cartesian-helper e (cdr set)))))


(define (my-fact-list n lst)
  (cond
    [(< n 0 ) lst]
    [#t (my-fact-list (- n 1) (cons (myfact n) lst))]))

(define (listoffact n)
  (myReverse (factlist 2 n '(1)) '()))

(define (factlist i n acc)
  (cond
    [(> i n ) acc]
    [#t (factlist (+ i 1) n (cons (* (car acc) i) acc))]))

;-----------------------------------------------------------

;odstran duplicity bezprostredne za sebou
(define (removeDup lst)
 (removeDupRec lst null))

(define (removeDupRec lst prev)
  (cond
    [(null? lst) lst]
    [(equal? prev (car lst)) (removeDupRec (cdr lst) prev)]
    [#t (cons (car lst) (removeDupRec (cdr lst) (car lst)))]
    ))

; ------------------------------------------------------------ bst

(define (bst-val bst)
  ( if (null? bst) -1 (car bst)))

(define (bst-left bst)
  (if (null? bst) -1 (cadr bst)))

(define (bst-right bst)
  (if (null? bst) -1 (caddr bst)))

(define (bst-find bst e)
  (cond
    [(empty? bst) #f]
    [(= (car bst) e) #t]
    [(> (car bst) e) (bst-find (cadr bst) e)]
    [#t (bst-find (caddr bst) e)]
    ))

(define (bst-insert bst e)
  (cond
    [(null? bst) (list e null null)]
    [(equal? e (car bst)) bst]
    [(< e (car bst)) (list (car bst) (bst-insert (cadr bst) e) (caddr bst))]
    [(> e (car bst)) (list (car bst) (cadr bst) (bst-insert (caddr bst) e))]
    ))

(define (bst-inorder bst)
  (cond
    [(empty? bst) '()]
    [#t (myAppendList (bst-inorder (cadr bst)) (cons (car bst) (bst-inorder (caddr bst))))]
    ))
    
 (define (bst-min bst)
   (cond
     [(null? bst) null]
     [(null? (cadr bst)) (car bst)]
     [#t (bst-min (cadr bst))]
     ))

 (define (bst-max bst)
   (cond
     [(null? bst) null]
     [(null? (caddr bst)) (car bst)]
     [#t (bst-max (caddr bst))]
     ))

(define (bst-height bst)
  (cond
    [(null? bst) -1]
    [#t (+ 1 (max (bst-height (cadr bst))  (bst-height (caddr bst))))]
    ))
 
;------------ Higher order functions --------------------------------------

(define (add a b)
  (+ a b))

;foldl

(define (my-foldl proc init lst)
  (if (null? lst) init
  (my-foldl proc (proc (car lst) init) (cdr lst))))

;foldr
(define (my-foldr proc init lst)
  (if (null? lst) init
      (proc (car lst) (my-foldr proc init (cdr lst)))))

;map using foldr
(define (my-map proc lst)
  (my-foldr (lambda (x acc) (cons (proc x) acc)) '() lst))

(define (myAppend2 x lst)
  (if (null? lst) (cons x '())
      (cons (car lst) (myAppend2 x (cdr lst) ))))

;map using foldl
(define (my-map2 proc lst)
  (my-foldl (lambda (x acc) (myAppend2 (proc x) acc)) '() lst))

;filter
(define (my-filter proc lst)
  (my-foldr (lambda (x acc) (if (proc x) (cons x acc) acc)) null lst))

(define (my-append2 lst e)
  (my-foldr (lambda (acc n) (cons n acc)) (cons e null) lst))


(define (fancyappend lst e)
  (my-foldr (lambda (x acc) (cons x acc)) (cons e null) lst))

(define (myReverse2 lst)
  (foldl (lambda (x acc) (cons x acc)) '() lst))

;--------------------------------------------------------------------------------


(define-macro (if-null? val tb fb)
  `(if (null? ,val)
       ,tb
       ,fb))

(define-macro (assert expr expected)
  `(if (equal? ,expr ,expected)
       (void)
       (println (format "Assertion ~a resulted in ~a but expected ~a" ',expr ,expr ,expected))))

;(assert (append '(1 2 ) '(3 4 5)) '(1 2 3 4))


(define-macro (my-let bind-pair body)
  `(
    (lambda ,(car bind-pair) ,body) ,(cadr bind-pair))
   )


;(my-let (a (+ 2 1)) (+ a 2))

(define-macro (my-let2 bind-pairs body)
  `(
    (lambda ,(map car bind-pairs ) ,body) ,@(map cadr bind-pairs)
   ))
   
(my-let2 ( [a  (+ 2 1)]  [b (+ 3 1)] [c (+ 3 3)] ) (+ (+ a b) c))

;---------------------- FW -------------------------------------------

;check list is aritmeticka posloupnost

(define (check-seq lst)
  (cond
    [(null? lst) #t]
    [(null? (cdr lst)) #t]
    [(> (car lst) (cadr lst)) #f]
    [#t (check-seq (cdr lst))]
    ))

(define (my-map3 proc lst)
  (if (null? lst) null
      (cons (proc (car lst)) (my-map3 proc (cdr lst)))))

(define (my-map4 proc lst)
  (foldr (lambda (x acc) (cons (proc x) acc)) '() lst))

(define (sumNeighbours lst)
  (cond
    [(or (null? lst) (null? (cdr lst))) lst]
    [#t (cons (+ (car lst) (cadr lst)) (sumNeighbours (cddr lst)))]
    ))

(define (reduce lst)
  (if (null? (cdr lst))
      (car lst)
      (reduce (sumNeighbours lst))))

(define (checkSeq lst)
  (cond
    [(or (null? lst) (null? (cdr lst))) 0]
    [(checkSeqRec lst (- (cadr lst) (car lst)))]
    ))

(define (checkSeqRec lst diff)
  (cond
    [(or (null? lst) (null? (cdr lst))) diff]
    [(= (- (cadr lst) (car lst)) diff) (checkSeqRec (cdr lst) diff)]
    [#t 0]
    ))

(define (fillSeq lst diff)
  (cond
    [(or (null? lst) (null? (cdr lst))) '()]
    [#t (let ((result (fillSeqRec lst diff -10)))
          (if (= (checkSeqRec result diff) 0)
              '()
              result))
   ]
    ))

(define (fillSeqRec lst diff prev)
  (cond
    [(null? lst) '()]
    [(= (car lst) 0) (cons (+ prev diff) (fillSeqRec (cdr lst) diff (+ prev diff)))]
    [#t (cons (car lst) (fillSeqRec (cdr lst) diff (car lst)))]
    ))
    
(define-macro (my-let3 bind-pairs body)
  `(
    (lambda ,(map car bind-pairs) ,body) ,@(map cadr bind-pairs)
    ))

(my-let3 ( [a  (+ 2 1)]  [b (+ 3 1)] [c (+ 3 3)] ) (+ (+ a b) c))










