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
        
        (proc-exp (var body)
          (proc-val (procedure var body env)))

        
        
     
        #| Q1A: 
        call-exp : Proc * ExpVal -> ExpVal
        didn't use the exterior function for practice reasons. 
        The function gets the proc-val and extracts the proc part.
        It then evaluates  the operand, and checks that the proc we've extracted was made by the only legal contractor.
        finally it evaluates the procedure with the current environment and uses the recursive nature of "value-of" to evaluate the procedure apllince. 
        |#
        
        (call-exp (rator rand)
                  (letrec ((proc1 (expval->proc (value-of rator env)))
                           (arg (value-of rand env)))
                           (cases proc proc1
                             (procedure (var body saved-env)
                                 (value-of body (extend-env var arg env))))))
        
        ;;mull-exp
        (mul-exp (exp1 exp2)
          (let ((val1 (value-of exp1 env))
                (val2 (value-of exp2 env)))
            (let ((num1 (expval->num val1))
                  (num2 (expval->num val2)))
              (num-val
                (* num1 num2))))) 
        
      
        #|  If we would like to be able to use Q2A proc as an exprestion.
        ;; istwice-exp


        (istwice-exp (id exp1 exp2)         
          (let ((val1 (value-of exp1 env))
                (val2 (value-of exp2 env)))
            (let ((num1 (expval->num val1))
                  (num2 (expval->num val2)))
              (if (zero? (- num1 (* num2 2)))                
                (bool-val #t)
                (bool-val #f)))))
        |#
      
)))
  

   
  ;; apply-procedure : Proc * ExpVal -> ExpVal
  ;; NOT IN USE 
  (define apply-procedure
    (lambda (proc1 val current-env)
      (cases proc proc1
        (procedure (var body saved-env)
          (value-of body (extend-env var val current-env))))))

  )
