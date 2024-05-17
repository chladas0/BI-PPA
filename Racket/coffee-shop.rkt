#lang racket

(define (ppa-max x y)
  (if (> x y) x y))

(define (inc x)
  ( + x 1))

(define (getCounters timeList prevTime streak counters)
  
  ; we reached the end of the list
  (if(empty? timeList)
     counters
     
   ; prevTime == curTime
   (if(equal? prevTime (car timeList))
      
       ;found new collision, update counters, add streak, continue
       (getCounters (cdr timeList) (car timeList) (inc streak) (ppa-max (inc streak) counters))
       
       ;breaked streak reset
       ( getCounters (cdr timeList) (car timeList) 1 counters))))

(define (coffee-shop timeList)
  (getCounters timeList '(-1 -1) 0 0))