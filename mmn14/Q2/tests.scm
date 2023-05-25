(module tests mzscheme
  
  (provide test-list)

  ;;;;;;;;;;;;;;;; tests ;;;;;;;;;;;;;;;;
  
  (define test-list
    '(
       
      ;;Q2A
      (istwice-proc_not "let istwice = proc (x) proc (y) zero?(-(x,-(y,-(0,y)))) in ((istwice 6) 3)" #t)
      (istwice-proc_not "let istwice = proc (x) proc (y) zero?(-(x,-(y,-(0,y)))) in ((istwice 3) 6)" #f)
      (istwice-proc_not "let istwice = proc (x) proc (y) zero?(-(x,-(y,-(0,y)))) in ((istwice 3) 4)" #f)
      
      
      ;;Q2C
      
      #|


let ishalf = proc (counter) proc (x) 
                                if zero? (x) then 0 
                                else if zero? (-(x,-(0,1))) then -(0,1)
                                else  -(((counter counter) -(x,2)) , -1)  
                                in let half = proc (x) ((ishalf ishalf) x)

in let ispower = proc (multiplior) 
                       proc (n)
                       proc (y)
                       proc(halfn)
                                if zero? (-(n,y))  ;;;;;;;if true it means n is 1 or power of 2
                                then zero? (0)                                 
                                else if zero?(halfn)  ;;;;; if we halfn is 0 it means we y is now greater then n so we can be sure n is not a power of 2
                                then zero?(1)                                
                                else ((( (multiplior multiplior)n) -(y , -(0,y)))(half halfn))  ;; multipal y by 2 and recheck when multiplior = ispower 
                                                                                                ;;and devide halfn as a break-cond
                                in let start = proc (n)
                                               if zero? (-(n,1)) 
                                               then zero?(0)
                                               else ((((ispower ispower)n) 2) -(n,-(0,n))) ;; init the multiplayor to 1 to make sure we compair only powers of 2 
                                                                                           ;; and break-cond to 2n so we can be sure wehn we passed n and found nouthing 
                                 in (start n)) ;; assign n with a const num val and get the aswer.


|#
      
      (powers-of-2-1 "let ishalf = proc (counter) proc (x) 
                                if zero? (x) then 0 
                                else if zero? (-(x,-(0,1))) then -(0,1)
                                else  -(((counter counter) -(x,2)) , -1)  
                                in let half = proc (x) ((ishalf ishalf) x)

in let ispower = proc (multiplior) 
                       proc (n)
                       proc (y)
                       proc(halfn)
                                if zero? (-(n,y)) 
                                then zero? (0)                                 
                                else if zero?(halfn)
                                then zero?(1)                                
                                else ((( (multiplior multiplior)n) -(y , -(0,y)))(half halfn)) 
                                in let start = proc (n)
                                               if zero? (-(n,1)) 
                                               then zero?(0)
                                               else ((((ispower ispower)n) 2) -(n,-(0,n)))
                                 in (start 5)" #f)
      
      (powers-of-2-2 "let ishalf = proc (counter) proc (x) 
                                if zero? (x) then 0 
                                else if zero? (-(x,-(0,1))) then -(0,1)
                                else  -(((counter counter) -(x,2)) , -1)  
                                in let half = proc (x) ((ishalf ishalf) x)

in let ispower = proc (multiplior) 
                       proc (n)
                       proc (y)
                       proc(halfn)
                                if zero? (-(n,y)) 
                                then zero? (0)                                 
                                else if zero?(halfn)
                                then zero?(1)                                
                                else ((( (multiplior multiplior)n) -(y , -(0,y)))(half halfn)) 
                                in let start = proc (n)
                                               if zero? (-(n,1)) 
                                               then zero?(0)
                                               else ((((ispower ispower)n) 2) -(n,-(0,n)))
                                 in (start 1024)" #t)
      
      (powers-of-2-3 "let ishalf = proc (counter) proc (x) 
                                if zero? (x) then 0 
                                else if zero? (-(x,-(0,1))) then -(0,1)
                                else  -(((counter counter) -(x,2)) , -1)  
                                in let half = proc (x) ((ishalf ishalf) x)

in let ispower = proc (multiplior) 
                       proc (n)
                       proc (y)
                       proc(halfn)
                                if zero? (-(n,y)) 
                                then zero? (0)                                 
                                else if zero?(halfn)
                                then zero?(1)                                
                                else ((( (multiplior multiplior)n) -(y , -(0,y)))(half halfn)) 
                                in let start = proc (n)
                                               if zero? (-(n,1)) 
                                               then zero?(0)
                                               else ((((ispower ispower)n) 2) -(n,-(0,n)))
                                 in (start 1)" #t)
      
      (powers-of-2-4 "let ishalf = proc (counter) proc (x) 
                                if zero? (x) then 0 
                                else if zero? (-(x,-(0,1))) then -(0,1)
                                else  -(((counter counter) -(x,2)) , -1)  
                                in let half = proc (x) ((ishalf ishalf) x)

in let ispower = proc (multiplior) 
                       proc (n)
                       proc (y)
                       proc(halfn)
                                if zero? (-(n,y)) 
                                then zero? (0)                                 
                                else if zero?(halfn)
                                then zero?(1)                                
                                else ((( (multiplior multiplior)n) -(y , -(0,y)))(half halfn)) 
                                in let start = proc (n)
                                               if zero? (-(n,1)) 
                                               then zero?(0)
                                               else ((((ispower ispower)n) 2) -(n,-(0,n)))
                                 in (start 0)" #f)
      
      
 #|
      ;; simple arithmetic
      (positive-const "11" 11)
      (negative-const "-33" -33)
      (simple-arith-1 "-(44,33)" 11)
  
      ;; nested arithmetic
      (nested-arith-left "-(-(44,33),22)" -11)
      (nested-arith-right "-(55, -(22,11))" 44)
  
      ;; simple variables
      (test-var-1 "x" 10)
      (test-var-2 "-(x,1)" 9)
      (test-var-3 "-(1,x)" -9)
      
      ;; simple unbound variables
      (test-unbound-var-1 "foo" error)
      (test-unbound-var-2 "-(x,foo)" error)
  
      ;; simple conditionals
      (if-true "if zero?(0) then 3 else 4" 3)
      (if-false "if zero?(1) then 3 else 4" 4)
      
      ;; test dynamic typechecking
      (no-bool-to-diff-1 "-(zero?(0),1)" error)
      (no-bool-to-diff-2 "-(1,zero?(0))" error)
      (no-int-to-if "if 1 then 2 else 3" error)

      ;; make sure that the test and both arms get evaluated
      ;; properly. 
      (if-eval-test-true "if zero?(-(11,11)) then 3 else 4" 3)
      (if-eval-test-false "if zero?(-(11, 12)) then 3 else 4" 4)
      
      ;; and make sure the other arm doesn't get evaluated.
      (if-eval-test-true-2 "if zero?(-(11, 11)) then 3 else foo" 3)
      (if-eval-test-false-2 "if zero?(-(11,12)) then foo else 4" 4)

      ;; simple let
      (simple-let-1 "let x = 3 in x" 3)

      ;; make sure the body and rhs get evaluated
      (eval-let-body "let x = 3 in -(x,1)" 2)
      (eval-let-rhs "let x = -(4,1) in -(x,1)" 2)

      ;; check nested let and shadowing
      (simple-nested-let "let x = 3 in let y = 4 in -(x,y)" -1)
      (check-shadowing-in-body "let x = 3 in let x = 4 in x" 4)
      (check-shadowing-in-rhs "let x = 3 in let x = -(x,1) in x" 2)

      ;; simple applications
      (apply-proc-in-rator-pos "(proc(x) -(x,1)  30)" 29)
      (apply-simple-proc "let f = proc (x) -(x,1) in (f 30)" 29)
      (let-to-proc-1 "(proc(f)(f 30)  proc(x)-(x,1))" 29)


      (nested-procs "((proc (x) proc (y) -(x,y)  5) 6)" -1)
      (nested-procs2 "let f = proc(x) proc (y) -(x,y) in ((f -(10,5)) 6)"
        -1)
      
      (y-combinator-1 "
let fix =  proc (f)
            let d = proc (x) proc (z) ((f (x x)) z)
            in proc (n) ((f (d d)) n)
in let
    t4m = proc (f) proc(x) if zero?(x) then 0 else -((f -(x,1)),-4)
in let times4 = (fix t4m)
   in (times4 3)" 12) |#
      ))
  )