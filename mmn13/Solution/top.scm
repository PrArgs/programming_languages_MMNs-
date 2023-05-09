(module top (lib "eopl.ss" "eopl")
  
  ;; top level module.  Loads all required pieces.
  ;; Run the test suite with (run-all).
  
  (require "utils.scm")
  (require "drscheme-init.scm")
  (require "data-structures.scm")  ; for expval constructors
  (require "lang.scm")             ; for scan&parse
  (require "interp.scm")           ; for value-of-program
  (require "tests.scm")            ; for test-list
  
  ;; since this is the top-level module, we don't really need to
  ;; provide anything, but we do so just in case.  

  (provide run run-all)

  ;; here are some other things that could be provided:

  ;;   (provide (all-defined))
  ;;   (provide (all-from "interp.scm"))
  ;;   (provide (all-from "lang.scm"))
  
  ;;;;;;;;;;;;;;;; interface to test harness ;;;;;;;;;;;;;;;;
  
  ;; run : String -> ExpVal
  ;; Page: 71
  (define run
    (lambda (string)
      (value-of-program (scan&parse string))))
  
  ;; run-all : () -> unspecified
  
  ;; runs all the tests in test-list, comparing the results with
  ;; equal-answer?  

  (define run-all
    (lambda ()
      (run-tests! run equal-answer? test-list)))
  
  (define equal-answer?
    (lambda (ans correct-ans)
      (equal? ans (sloppy->expval correct-ans))))
  
  (define sloppy->expval 
    (lambda (sloppy-val)
      (cond
        ((number? sloppy-val) (num-val sloppy-val))
        ((boolean? sloppy-val) (bool-val sloppy-val))
        (else
         (eopl:error 'sloppy->expval 
                     "Can't convert sloppy value to expval: ~s"
                     sloppy-val)))))
    
  ;; run-one : symbol -> expval

  ;; (run-one sym) runs the test whose name is sym
  
  (define run-one
    (lambda (test-name)
      (let ((the-test (assoc test-name test-list)))
        (cond
          ((assoc test-name test-list)
           => (lambda (test)
                (run (cadr test))))
          (else (eopl:error 'run-one "no such test: ~s" test-name))))))
 
  ;; (run-all)
    
;-------------------------------------------------------------------------
   ;Unit tests
  (display "___Unit Testing___\n\n")
  
  
 (display "- Q1a -\n")
    
  (equal?? (run " +(2,3)") (num-val 5)) 
  (equal?? (run " +(+(2,3) , 3)") (num-val 8))
  (equal?? (run " +(5 , +(2,3))") (num-val 10)) 
  (equal?? (run " +(+(5,2) , +(2,3))") (num-val 12))
  
  (equal?? (run " *(2,3)") (num-val 6)) 
  (equal?? (run " *(+(2,3) , 3)") (num-val 15))
  (equal?? (run " *(5 , *(2,3))") (num-val 30))
  (equal?? (run " *(+(5,2) , +(2,3))") (num-val 35))
 
  (equal?? (run " /(6,3)") (num-val 2)) 
  (equal?? (run " +(/(9,3) , 3)") (num-val 6))
  (equal?? (run " /(10 , +(2,3))") (num-val 2))
  (equal?? (run " /(+(5,3) , +(2,2))") (num-val 2))

  (report-unit-tests-completed 'Q1a)
  
  ;-------------------------------------------------------------------------
  
  (display "- Q1b -\n")
  
  
  (equal?? (run " greater?(2,3)") (bool-val #f)) 
  (equal?? (run " greater?(+(2,3) , 3)") (bool-val #t))
  (equal?? (run " greater?(5 , +(2,3))") (bool-val #f)) 
  (equal?? (run " greater?(+(5,2) , +(2,3))") (bool-val #t))
  
  (equal?? (run " less?(2,3)") (bool-val #t)) 
  (equal?? (run " less?(+(2,3) , 3)") (bool-val #f))
  (equal?? (run " less?(5 , *(2,3))") (bool-val #t))
  (equal?? (run " less?(+(5,2) , +(2,3))") (bool-val #f))
  
  (equal?? (run " equal?(2,3)") (bool-val #f)) 
  (equal?? (run " equal?(+(2,3) , 3)") (bool-val #f))
  (equal?? (run " equal?(5 , +(2,3))") (bool-val #t))
  (equal?? (run " equal?(+(5,2) , +(2,3))") (bool-val #f))
  
  (report-unit-tests-completed 'Q1b)
  
;-------------------------------------------------------------------------
  
  (display "- Q2a -\n")
  
  
  (equal?? (run " cons(2,3)") (cons-val (num-val 2) (num-val 3))) 
  (equal?? (run " cons(cons(2,3) , 3)") (cons-val (cons-val (num-val 2) (num-val 3)) (num-val 3)))
  (equal?? (run " cons(5 , cons(2,3))") (cons-val (num-val 5) (cons-val(num-val 2) (num-val 3)))) 
  (equal?? (run " cons(cons(5,2) , cons(2,3))") (cons-val (cons-val (num-val 5) (num-val 2)) (cons-val (num-val 2)(num-val 3))))
  (equal?? (run " cons(2, cons( 3, cons( 4, emptylist)))") (cons-val (num-val 2) (cons-val (num-val 3) (cons-val (num-val 4) (emptylist-val)))))
  
  (equal?? (run " car( cons(2, cons( 3, cons( 4, emptylist))))") (num-val 2)) 
  (equal?? (run " cdr( cons(2, cons( 3, cons( 4, emptylist))))") (cons-val (num-val 3) (cons-val (num-val 4) (emptylist-val))))
  (equal?? (run " car(cdr( cons(2, cons( 3, cons( 4, emptylist)))))") (num-val 3))
  
  (equal?? (run " null?(cons(0,emptylist))") (bool-val #f))
  (equal?? (run " null?(emptylist)") (bool-val #t))
  (equal?? (run " null?(cdr(cons(0,emptylist)))") (bool-val #t))
  
   (equal?? (run " let x = 4 
                    in cons(x,
                      cons(cons(-(x,1),
                           emptylist),
                     emptylist))") (cons-val (num-val 4) (cons-val (cons-val (num-val 3) (emptylist-val)) (emptylist-val))))
  
  
  (equal?? (run " emptylist") (emptylist-val))
  
  (report-unit-tests-completed 'Q2a)
  
  ;-------------------------------------------------------------------------

  (display "- Q2b -\n")

  
  (equal?? (run " let x = 4
                    in list(x, -(x,1), -(x,3))") (cons-val (num-val 4) (cons-val (num-val 3) (cons-val (num-val 1) (emptylist-val)))))
  
  (equal?? (run " car(list (15, 6, 7, 8 ,9))") (num-val 15)) 
  (equal?? (run " cdr(list (15, 6, 7))") (cons-val (num-val 6) (cons-val (num-val 7) (emptylist-val))))
  (equal?? (run " car(cdr( list ( 15, 6, 7, 8 ,9)))") (num-val 6))
 
  (report-unit-tests-completed 'Q2b)

  ;-------------------------------------------------------------------------

  
  )




