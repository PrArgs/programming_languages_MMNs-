(module interp (lib "eopl.ss" "eopl")
  
  ;; interpreter for the LET language.  The \commentboxes are the
  ;; latex code for inserting the rules into the code in the book.
  ;; These are too complicated to put here, see the text, sorry.

  (require "drscheme-init.scm")

  (require "lang.scm")
  (require "data-structures.scm")
  (require "environments.scm")

  (provide value-of-program value-of)

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

  ;; value-of-program : Program -> ExpVal
  ;; Page: 71
  (define value-of-program 
    (lambda (pgm)
      (cases program pgm
        (a-program (exp1)
          (value-of exp1 (init-env))))))

  ;; value-of : Exp * Env -> ExpVal
  ;; Page: 71
  (define value-of
    (lambda (exp env)
      (cases expression exp

        ;\commentbox{ (value-of (const-exp \n{}) \r) = \n{}}
        (const-exp (num) (num-val num))

        ;\commentbox{ (value-of (var-exp \x{}) \r) = (apply-env \r \x{})}
        (var-exp (var) (apply-env env var))

        ;\commentbox{\diffspec}
        (diff-exp (exp1 exp2)                  
          (let ((val1 (value-of exp1 env))
                (val2 (value-of exp2 env)))
            (let ((num1 (expval->num val1))
                  (num2 (expval->num val2)))
              (num-val
                (- num1 num2)))))
        
;;;;;;;;;;;;;;;;;;;;;;;;;;MMN13 PART HERE;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Q1 : Adding add-exp , mul-exp, quot-exp
        ;
        ;\commentbox{\addspec}
        (add-exp (exp1 exp2)                  
          (let ((val1 (value-of exp1 env))
                (val2 (value-of exp2 env)))
            (let ((num1 (expval->num val1))
                  (num2 (expval->num val2)))
              (num-val
               (+ num1 num2)))))
        
        ;\commentbox{\mulspec}
        (mul-exp (exp1 exp2)                  
          (let ((val1 (value-of exp1 env))
                (val2 (value-of exp2 env)))
            (let ((num1 (expval->num val1))
                  (num2 (expval->num val2)))
              (num-val
                (* num1 num2)))))
        
        ;\commentbox{\devspec}
        (quot-exp (exp1 exp2)                  
          (let ((val1 (value-of exp1 env))
                (val2 (value-of exp2 env)))
            (let ((num1 (expval->num val1))
                  (num2 (expval->num val2)))
              ( if (zero? num2)
                   (eopl:error 'quot-exp "We dont divide byt zero ~s" num2)
                   (num-val (floor (/ num1 num2)))))))
        
        
        ;\commentbox{\zerotestspec}
        (equal?-exp (exp1 exp2)                  
          (let ((val1 (value-of exp1 env))
                (val2 (value-of exp2 env)))
            (let ((num1 (expval->num val1))
                  (num2 (expval->num val2)))
              (if (equal? num1 num2)
                (bool-val #t)
                (bool-val #f)))))
        
        ;\commentbox{\zerotestspec}
        (greater?-exp (exp1 exp2)                  
          (let ((val1 (value-of exp1 env))
                (val2 (value-of exp2 env)))
            (let ((num1 (expval->num val1))
                  (num2 (expval->num val2)))
              (if (> num1 num2)
                (bool-val #t)
                (bool-val #f)))))
        
        ;\commentbox{\zerotestspec}
        (less?-exp (exp1 exp2)                  
          (let ((val1 (value-of exp1 env))
                (val2 (value-of exp2 env)))
            (let ((num1 (expval->num val1))
                  (num2 (expval->num val2)))
              (if (< num1 num2)
                (bool-val #t)
                (bool-val #f)))))        
      
;;;;;;;Q2A;;;;;;;;;
        
        ;\commentbox{\constestspec}
        (cons-exp (exp1 exp2)                  
          (let ((val1 (value-of exp1 env))
                (val2 (value-of exp2 env)))
                   (cons-val val1 val2)))
        
         ;\commentbox{\carestspec}
        (car-exp (exp1)                  
          (let ((val1 (value-of exp1 env)))
                   (expval->car val1)))
        
         ;\commentbox{\cdrtestspec}
        (cdr-exp (exp1)                  
          (let ((val1 (value-of exp1 env)))
                   (expval->cdr val1)))
        
         ;emptylist check
         ;\commentbox{\nulltestspec}
        (null?-exp (exp1)                  
          (let ((val1 (value-of exp1 env)))
             (let ((bool(expval->null? val1)))
               (bool-val bool))))
        
        ;\commentbox{\emptylisttestspec}
        (emptylist-exp ()
                       (emptylist-val))       
      
;;;;;;;Q2A;;;;;;;;;
        
        ;\commentbox{\listtestspec}       
        ;uses foldr to list the expretions
        (list-exp (exps)
          (my_foldr (emptylist-val) (map (lambda (expr) (value-of expr env)) exps)))

;;;;;;;Q3;;;;;;;;;
        
        ;\commentbox{\Arraytestspec} 
        ; arry represented by list
        (array-exp (exps)
          (my_foldr (emptylist-val) (map (lambda (expr) (value-of expr env)) exps)))
        
         ;\commentbox{\Indextestspec} 
        ; works as arry[i] in OOP lang 
        (index-exp (array index)
                 (define (index-iterator array index counter)
                     (if (expval->null? array) (eopl:error "Index out of range")
                       (if (equal? counter (expval->num (value-of index env)))
                           (expval->car array)
                           (index-iterator (expval->cdr array) index (+ counter 1)))))
                               (index-iterator (value-of array env) index 1))
                 
        

        
      
        
        
;;;;;;;;;;;;;;;;;;;;;;;;;;MMN13 PART ENDS HERE;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

        ;\commentbox{\zerotestspec}
        (zero?-exp (exp1)
          (let ((val1 (value-of exp1 env)))
            (let ((num1 (expval->num val1)))
              (if (zero? num1)
                (bool-val #t)
                (bool-val #f)))))
              
        ;\commentbox{\ma{\theifspec}}
        (if-exp (exp1 exp2 exp3)
          (let ((val1 (value-of exp1 env)))
            (if (expval->bool val1)
              (value-of exp2 env)
              (value-of exp3 env))))

        ;\commentbox{\ma{\theletspecsplit}}
        (let-exp (vars exps body)       
          (let ((vals (values-of-exps exps env)))
            (value-of body
              (extend-env vars vals env))))
            
            
        
        (let*-exp (vars exps body)
         (if (null? vars) 
           (value-of body env)
           (let* ((v1 (value-of (car exps) env))
                  (e1 (extend-env (list (car vars)) (list v1) env)))
            (value-of (let*-exp (cdr vars) (cdr exps) body) e1))))
        

        )))
  

;since eopl does not have foldr func i implimnted one myself
  (define my_foldr 
         (lambda (init list)
           (if (null? list)
               init
               (cons-val (car list) (my_foldr init (cdr list))))))
  
  
(define values-of-exps
    (lambda (exps env)
      (map (lambda(x) (value-of x env)) exps)))
  )

