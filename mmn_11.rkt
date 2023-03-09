#lang scheme

#|
this is the Work product of Roi Argaman
Q1.a : My_append 
|#
(define my_append
  (lambda (lst1 lst2)
    (if (not (empty? lst1))
        (cons (car lst1) (my_append (cdr lst1) lst2))
        (if (empty? lst2)
            '()
            (cons (car lst2) (my_append  lst1 (cdr lst2)))
            ))))



        
        
 
;Q1.b : My_append no recursion 


