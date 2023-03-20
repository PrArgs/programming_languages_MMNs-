#lang scheme

(require "utils.scm")

#|
this is the Work product of Roi Argaman
Q1.a : My_append 
append 2 list into 1 with recursion
|#
(define my_append
  (lambda (lst1 lst2)
    (if (not (empty? lst1))
        (cons (car lst1) (my_append (cdr lst1) lst2))
        (if (empty? lst2)
            '()
            (cons (car lst2) (my_append  lst1 (cdr lst2)))
            ))))       
        
 
;Q1.b : My_append 
; append 2 list into 1 without recursion

(define (my_append_fr lst1 lst2)
    (foldr cons lst2 lst1))    


#|
Q2 : filter
 returns the items in the given list that are 
 compatible with the given predicate
|#

(define filterl
  (lambda (pred lst)
    (foldr (lambda (i result)
             (if (pred i)
                 (cons i result)
                 result))
          '() lst
             )))




#|
Q3 : set-dif 
|#

;checks if an item is in given list 
(define contains
   (λ (lst x)
     (if (empty? lst)
         #f
         (if (equal? (car lst) x)
             #t
             (contains (cdr lst) x)
             ))))

; subtracting list2 from list1
(define sub
   (λ (lst1 lst2)
      (foldr (lambda (i result)
             (if (not(contains lst1 i))
                 (cons i result)
                 result))
      '() lst2)))
 
;returns the symmetric difference by using the Propertie 
;is equivalent to the union of both relative complements A/\B = (A\B)U(B\A)
;uses append but can easly use my_append
(define set-dif
  (λ (lst1 lst2)    
    (append (sub lst1 lst2) (sub lst2 lst1))))


#|
Q4 : complete foo
|#
(define foo
  (lambda (ls s)
    (cond
      [(null? ls) `(() . ,s)]
      [(pair? (car ls))
       (let ((p (foo (car ls) s)))
         (let ((p1 (foo (cdr ls) (cdr p))))
           ;if we got a pair p is the first iteam and p1 is the second 
           `(,(cons (car p) (car p1)) . ,(cdr p1))))]
      [(or (null? (car ls)) (odd? (car ls)))
       (let ((p (foo (cdr ls) s)))
         ;if we got to the end of a list or got odd number we will add it (car ls) and keep exploring (car p)
         `(,(cons (car ls) (car p)) . ,(cdr p)))]
      [else (let ((p (foo (cdr ls) s)))
              `(,(car p) . ,(add1 (cdr p))))])))




display "___________TESTS___________"


(display "---TEST Q1---\n")
(sleep 0.5)
  (let ([l'()] [l1 '(a b c)] [l2 '(x y z)] [l3 '(ggs ppl 111)] 
               [l12 '(a b c x y z)] 
               [l23 '(x y z ggs ppl 111)])
    (equal?? l (my_append l l))
    (equal?? l1 (my_append l l1))
    (equal?? l1 (my_append l1 l))
    (equal?? l12 (my_append l1 l2))
    (equal?? l23 (my_append l2 l3))
    (report-unit-tests-completed 'my_append)
    
(display "-------------------------------------------------------------------------------- \n")
    (equal? l (my_append_fr l l))
    (equal? l1 (my_append_fr l l1))
    (equal? l1 (my_append_fr l1 l))
    (equal? l12 (my_append_fr l1 l2))
    (equal? l23 (my_append_fr l2 l3))
    (report-unit-tests-completed 'my_append_fr)
    )

(sleep 2)
(display "--------------------------------------------------------------------------------")

(display "\n---TEST Q2---\n")
  (let ([l'()] [l1 '(1 2 3 4 5 6)] [l2 '(543 234 121)] [l3 '(3 7.8 190)]
               [l1c '(2 4 6)] [l3c '(190)])
    (equal? l (filterl even? l))
    (equal? l1c (filterl even? l1))
    (equal? l3c (filterl even? l3c))
    (equal? '(234) (filterl even? l2))
    (report-unit-tests-completed 'filterl)
    )
(sleep 2)
(display "--------------------------------------------------------------------------------")

(display "\n---TEST Q3---\n")
  (let ([l'()] [l1 '(a b c d)] [l2 '(x b d w)] [l3 '(x X)]
               [l32 '(b d w X)] [l3c '()])
    (equal?? l (set-dif l l))
    (equal?? l1 (set-dif l l1))
    (equal?? l32 (set-dif l3 l2))
    (report-unit-tests-completed 'set-dif)
    )    
  (sleep 2)
 
(display "----------------------------------------------------------------------- \n")

(sleep 2)
(display "---TEST Q4---\n")
(define l '(2 3 (7 4 5 6) 8 (9) 2))
  (equal?? (foo l 0) '((3 (7 5) (9)) . 5))
  (report-unit-tests-completed 'foo)


                





