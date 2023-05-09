(module tests mzscheme
  
  (provide test-list)

  ;;;;;;;;;;;;;;;; tests ;;;;;;;;;;;;;;;;
  
  (define test-list
    '(
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
      
|#
      
      ;;Q1A
      ;;checking addition 
      (simp-add " +(-1,2)" 1)
      (comp-add" +(+(-1,3) , 3)" 5)
      (comp-add2 " +(-5 , +(2,1))" -2)
      
      ;checking multipication
      (simp-mul " *(1,2)" 2)
      (neg-one-nul " *(-1,2)" -2)
      (zero-comp-mull " *(+(682,4153) , 0)" 0)
      
      ;checking devition 
      (simp-dev " /(3,3)" 1)
      (zero_dev " /(2131,0)" error)
      (comp-dev " /(4 , +(3,3))" 0)
      (one-comp-dev " /(68, +(/(4,1) ,4))" 8)
      
      ;Q1B
      ;less
      (simp-less "less? (/(3,3) , 1)" #f)
      (zero-less "less? (2131,0)" #f)
      (comp-less "less? (4 , +(3,-3))" #f)
      (sec-comp-less "less? (+(/(4,1) ,4), 68)" #t)
      
      ;equal
      (simp-equal "equal? (/(3,3) , 1)" #t)
      (zero-equal "equal? (2131,0)" #f)
      (comp-equal "equal? (4 , +(3,-3))" #f)
      (sec-comp-equal "equal? (+(/(4,1) ,4), 68)" #f)
      
       ;greater
      (simp-greater "greater? (/(3,3) , 1)" #f)
      (zero-greater "greater? (2131,0)" #t)
      (comp-greater "greater? (4 , +(3,-3))" #t)
      (sec-comp-greater "greater? (+(/(4,1) ,4), 68)" #f)
      
      ;;Q2 is at top.scm
      
      ;;check Array Q3
      (check-arry-by-book " let A = array {10, -(5,7) , zero?(8), array {1,2,3}, 12 } in
                         -(<A>[1] , <<A>[4]>[2])" 8)

      )))