;;;  zip 2 lists using recursion, map,foldr 
#lang racket

(require "utils.scm")

(define (zipWithRec lst1 lst2)
  (if (null? lst1)
      '()
      (cons (list (car lst1) (car lst2)) (zipWithRec (cdr lst1) (cdr lst2)))))
  
  
;-------------------------------------------------------------------------
(define (zipWithMap lst1 lst2) (map list lst1 lst2)) 
;-------------------------------------------------------------------------
(define (zipWithFoldr lst1 lst2)
  (foldr (lambda (a b result)
    (cons (list a b) result))
    `()
    lst1
    lst2)
)
;-------------------------------------------------------------------------
;Unit tests

(equal?? (zipWithRec '(1 2 3 4) '(5 6 7 8))  '((1 5) (2 6) (3 7) (4 8)))
(equal?? (zipWithRec '(1 (3 4) ) '(5  "xyz"))  '((1 5) ((3 4) "xyz") ))
(report-unit-tests-completed 'zipWithRec)

(equal?? (zipWithMap '(1 2 3 4) '(5 6 7 8))  '((1 5) (2 6) (3 7) (4 8)))
(equal?? (zipWithMap '(1 (3 4) ) '(5  "xyz"))  '((1 5) ((3 4) "xyz") ))
(report-unit-tests-completed 'zipWithMap)

(equal?? (zipWithFoldr '(1 2 3 4) '(5 6 7 8))  '((1 5) (2 6) (3 7) (4 8)))
(equal?? (zipWithFoldr '(1 (3 4) ) '(5  "xyz"))  '((1 5) ((3 4) "xyz") ))
(report-unit-tests-completed 'zipWithFoldr)