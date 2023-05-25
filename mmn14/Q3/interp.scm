(module interp (lib "eopl.ss" "eopl")
  
  ;; interpreter for the PROC language, using the data structure
  ;; representation of procedures.

  ;; The \commentboxes are the latex code for inserting the rules into
  ;; the code in the book. These are too complicated to put here, see
  ;; the text, sorry. 

  (require "drscheme-init.scm")

  (require "lang.scm")
  (require "data-structures.scm")
  (require "environments.scm")

  (provide value-of-program value-of)

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

  ;; value-of-program : Program -> ExpVal
  (define value-of-program 
    (lambda (pgm)
      (cases program pgm
        (a-program (exp1)
          (value-of exp1 (init-env))))))

  ;; value-of : Exp * Env -> ExpVal
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
        (let-exp (var exp1 body)       
          (let ((val1 (value-of exp1 env)))
            (value-of body
              (extend-env var val1 env))))
        
        
        ;;;;;
        
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
      

        
        ;\commentbox{\listtestspec}       
        ;uses foldr to list the expretions
        (list-exp (exps)
          (my_foldr (emptylist-val) (map (lambda (expr) (value-of expr env)) exps)))
        
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
                 
        


        
        (proc-exp (vars values body)
                  (let ((ids (my_foldr (emptylist-val) vars))
                        (defualts (my_foldr (emptylist-val) (map (lambda (expr) (value-of expr env)) values)))) 
          (proc-val (procedure ids defualts body env))))

        (call-exp (rators ids rands)
          (letrec ((new-lists (checkwhatcahnged(rators rands)))
                   (procs (car new-lists))
                   (args (cdr new-lists)))
            (apply-procedure procs args env)))

        )))
  
  ;since eopl does not have foldr func i implimnted one myself
  (define my_foldr 
         (lambda (init list)
           (if (null? list)
               (cons-val (car list) (my_foldr init (cdr list))))))
  
  (define values-of-exps
    (lambda (exps env)
      (map (lambda(x) (value-of x env)) exps)))
  
  (define add-valus
    (lambda (ids exps body)
      (if (null? (car ids))
          (body)          
          (
           (value-of body
              (extend-env (car ids) (value-of (car exps) body) body))
              (add-valus (cdr ids) (cdr exps) body)))))
          
  
  (define checkwhatcahnged
    (lambda (rators rands)
    (cons rators (cons rands '()))))

  ;; apply-procedure : Proc * ExpVal -> ExpVal
  ;; Page: 79
  (define apply-procedure
    (lambda (procs args env)
      (cases proc procs
        (procedure (vars exsps body saved-env)
                   ;;;lopping over to extend inviorment with current valus of all
                   (letrec ((new-env (add-valus (vars exsps env))))                   
          (value-of body (extend-env exsps new-env)))))))

  )
