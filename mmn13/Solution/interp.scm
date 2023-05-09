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
      
;;;;;;;Q2.A;;;;;;;;;
        
        ;\commentbox{\constestspec}
        (cons-exp (exp1 exp2)                  
          (let ((val1 (value-of exp1 env))
                (val2 (value-of exp2 env)))
                   (cons-val val1 val2)))
        
        
        (car-exp (exp1)                  
          (let ((val1 (value-of exp1 env)))
                   (expval->car val1)))
        
        
        (cdr-exp (exp1)                  
          (let ((val1 (value-of exp1 env)))
                   (expval->cdr val1)))
        
        
        (null?-exp (exp1)                  
          (let ((val1 (value-of exp1 env)))
             (let ((bool(expval->null? val1)))
               (bool-val bool))))
        
        
        (emptylist-exp ()
                       (emptylist-val))
        
        
        ;uses foldr to list the expretions
        (list-exp (exps)
         (my_foldr cons-val (emptylist-val) (map (lambda (expr) (value-of expr env)) exps)))
                 
        

        
      
        
        
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
         (lambda (func init list)
           (if (null? list)
               init
               (func (car list) (my_foldr func init (cdr list))))))
  
  
(define values-of-exps
    (lambda (exps env)
      (map (lambda(x) (value-of x env)) exps)))
  )

