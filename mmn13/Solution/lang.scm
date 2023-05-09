(module lang

  ;; grammar for the LET language

  (lib "eopl.ss" "eopl")                
  
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
;;;;;;;;;;;;;;;;;;;;;;;;;;MMN13 PART HERE;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      
      (expression
        ("+" "(" expression "," expression ")")
        add-exp)
      
      
      (expression
        ("*" "(" expression "," expression ")")
        mul-exp)
      
      
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
      
      (expression 
       ("list" "(" (separated-list expression ",") ")") 
       list-exp)
      
      ; expression parser for creating an array Q3
      (expression 
       ("array" "{" (separated-list expression ",") "}" )
       array-exp)
      
      ; expression parser for extracting a value out of an array at an index Q3
      (expression
       ("<" expression ">" "[" expression "]")
       index-exp)
      
      
      
;;;;;;;;;;;;;;;;;;;;;;;;;;MMN13 PART ENDS HERE;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;      
      
      (expression
       ("zero?" "(" expression ")")
       zero?-exp)

      (expression
       ("if" expression "then" expression "else" expression)
       if-exp)

      (expression (identifier) var-exp)

      (expression
       ("let" (arbno identifier "=" expression) "in" expression)
       let-exp)
      
      (expression
       ("let*" (arbno identifier "=" expression) "in" expression)
       let*-exp)

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
