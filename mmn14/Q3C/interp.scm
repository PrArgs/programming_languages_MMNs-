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
        
        (proc-exp (ids defs body)                   
          (proc-val (procedure ids defs body env)))

        (call-exp (rator parms exps)                 
          (let ((proc (expval->proc (value-of rator env))))
            (apply-procedure proc parms exps env)))

        )))

  ;; apply-procedure : Proc * ExpVal -> ExpVal
  ;; Page: 79
  (define apply-procedure
    (lambda (proc1 new_ids new_exps new_env)
      (cases proc proc1
        (procedure (vars defs body saved-env)
        (if (is-ligal vars new_ids)
            (cond ((not (null? new_ids))
                   (display "new ids is not null")
                   (value-of body (extend-all vars defs new_env)))
                  (else
                   (display (not (null? new_ids)))
             (append vars new_ids)
            (append defs new_exps)
            (if (null? vars)
             (value-of body new_env)             
             (value-of body (extend-all vars defs new_env)))))
            (eopl:error 'apply-procedure "You can't assing new vars"))
            ))))
  
  
  
  (define is-ligal
    (lambda (old_vars new_ids)
      (if (> (length new_ids) (length old_vars))
             (eopl:error 'is-ligal "New list is too long")
             (cond ((null? new_ids) #t) ; If List 1 is empty, return true
                   ((member (car new_ids) old_vars) ; If the first item of newp  is in oldv
                    (is-ligal (cdr new_ids) old_vars)); Recursively check the remaining items
                   (else #f); we have an new var that is not in the original list
                   ))))  

  (define extend-all 
    (lambda (vars defs new-env)
      (map (lambda (exp) (extend-env exp new-env)) defs)
      (display "\n")
           (display vars)
           (display "\n")
           (display defs)
           (display "\n")      
      (if (null? (cdr vars))
          (extend-env (car vars) (car defs) new-env)
          (extend-all (cdr vars) (cdr defs) (extend-env (car vars) (car defs) new-env)))))            

  )