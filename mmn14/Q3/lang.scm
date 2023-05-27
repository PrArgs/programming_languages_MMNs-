(module lang (lib "eopl.ss" "eopl")                

  ;; grammar for the PROC language
  
  (require "drscheme-init.scm")
  
  (provide (all-defined))

  ;;;;;;;;;;;;;;;; grammatical specification ;;;;;;;;;;;;;;;;
  
  (define the-lexical-spec
    '((whitespace (whitespace) skip)
      (comment ("%" (arbno (not #\newline))) skip)
      (identifier
       (letter (arbno (or letter digit "_" "-" "?")))
       symbol)
      (number (digit (arbno digit)) number)
      (number ("-" digit (arbno digit)) number)
      ))
  
  (define the-grammar
    '((program (expression) a-program)

      (expression (number) const-exp)
      (expression
        ("-" "(" expression "," expression ")")
        diff-exp)
      
      (expression
       ("zero?" "(" expression ")")
       zero?-exp)

      (expression
       ("if" expression "then" expression "else" expression)
       if-exp)

      (expression (identifier) var-exp)

      (expression
       ("let" identifier "=" expression "in" expression)
       let-exp)
      
;;;;;;;;;;;;;;;;;;;;;;;;;;MMN13 PART HERE;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      ;add exp
      (expression
        ("+" "(" expression "," expression ")")
        add-exp)
           
      ;mull exp      
      (expression
        ("*" "(" expression "," expression ")")
        mul-exp)
           
      ;dev exp     
      (expression
        ("/" "(" expression "," expression ")")
        quot-exp)
      
      (expression
        ("equal?" "(" expression "," expression ")")
       equal?-exp)
      
      (expression
        ("greater?" "(" expression "," expression ")")
        greater?-exp)
      
      (expression
        ("less?" "(" expression "," expression ")")
        less?-exp)
      
;;;;;;;Q2.A;;;;;;;;;
      
      (expression
        ("cons" "(" expression "," expression ")")
        cons-exp)
      
      (expression
       ("car" "(" expression ")")
       car-exp)
      
      (expression
       ("cdr" "(" expression ")")
       cdr-exp)
      
      (expression
       ("null?" "(" expression ")")
       null?-exp)
      
      (expression
       ("emptylist")
       emptylist-exp)
      
;;;;;;;Q2.B;;;;;;;;;      
      (expression 
       ("list" "(" (separated-list expression ",") ")") 
       list-exp)
      
;;;;;;;Q3;;;;;;;;;      
      
      ; syntax for creating an array 
      (expression 
       ("array" "{" (separated-list expression ",") "}" )
       array-exp)
      
      ; syntax for value extraction from specific index of an array  
      (expression
       ("<" expression ">" "[" expression "]")
       index-exp)
      
      
      
;;;;;;;;;;;;;;;;;;;;;;;;;;MMN13 PART ENDS HERE;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      

      (expression
       ("proc" "(" (separated-list identifier "=" expression ",") ")" expression)
       proc-exp)

      (expression
       ("(" expression (separated-list identifier "=" expression ",") ")")
       call-exp)
      
      ))

  ;;;;;;;;;;;;;;;; sllgen boilerplate ;;;;;;;;;;;;;;;;
  
  (sllgen:make-define-datatypes the-lexical-spec the-grammar)
  
  (define show-the-datatypes
    (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))
  
  (define scan&parse
    (sllgen:make-string-parser the-lexical-spec the-grammar))
  
  (define just-scan
    (sllgen:make-string-scanner the-lexical-spec the-grammar))
  
  )
