(module interp (lib "eopl.ss" "eopl")
  
  ;; interpreter for the EXPLICIT-REFS language

  (require "drscheme-init.scm")

  (require "lang.scm")
  (require "data-structures.scm")
  (require "environments.scm")
  (require "store.scm")
  
  (provide value-of-program value-of instrument-let instrument-newref)

;;;;;;;;;;;;;;;; switches for instrument-let ;;;;;;;;;;;;;;;;

  (define instrument-let (make-parameter #f))

  ;; say (instrument-let #t) to turn instrumentation on.
  ;;     (instrument-let #f) to turn it off again.

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

  ;; value-of-program : Program -> ExpVal
  ;; Page: 110
  (define value-of-program 
    (lambda (pgm)
      (initialize-store!)               ; new for explicit refs.
      (cases program pgm
        (a-program (exp1)
          (value-of exp1 (init-env))))))

  ;; value-of : Exp * Env -> ExpVal
  ;; Page: 113
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

        (call-exp (rator rand)
          (let ((proc (expval->proc (value-of rator env)))
                (arg (value-of rand env)))
            (apply-procedure proc arg)))

        (letrec-exp (p-names b-vars p-bodies letrec-body)
          (value-of letrec-body
            (extend-env-rec* p-names b-vars p-bodies env)))

        (begin-exp (exp1 exps)
          (letrec 
            ((value-of-begins
               (lambda (e1 es)
                 (let ((v1 (value-of e1 env)))
                   (if (null? es)
                     v1
                     (value-of-begins (car es) (cdr es)))))))
            (value-of-begins exp1 exps)))

        (newref-exp (exp1)
          (let ((v1 (value-of exp1 env)))
            (ref-val (newref v1))))

        (deref-exp (exp1)
          (let ((v1 (value-of exp1 env)))
            (let ((ref1 (expval->ref v1)))
              (deref ref1))))

        (setref-exp (exp1 exp2)
          (let ((ref (expval->ref (value-of exp1 env))))
            (let ((v2 (value-of exp2 env)))
              (begin
                (setref! ref v2)
                (num-val 23)))))
        
        (arr-exp (typ size first rest)
          (letrec (
                (values (map (lambda (exp) (value-of exp env)) (cons first rest)))
                (Alength (expval->num (value-of size env)))) 
            (if (equal? Alength (length values))
                  (arr-val typ Alength (legal-list typ values '()))                  
                   (eopl:error 'value-of "Miss match length given ~v values and array length is ~s" (length values) Alength))))
        
        (index-exp (arrexp index)
          (let* ((array1 (expval->arr (value-of arrexp env)))
                (i (expval->num (value-of index env))))
                (cases arr array1
                  (array (typ lengthA arry)
                         (if (> lengthA i -1)
                             (let ((val (get-val arry i)))
                               (value-of val env)
                             (eopl:error 'value-of "Ilegal index ~s" i)))))))
        )))

  ;; apply-procedure : Proc * ExpVal -> ExpVal
  ;; 
  ;; uninstrumented version
  ;;   (define apply-procedure
  ;;    (lambda (proc1 arg)
  ;;      (cases proc proc1
  ;;        (procedure (bvar body saved-env)
  ;;          (value-of body (extend-env bvar arg saved-env))))))

  ;; instrumented version
  (define apply-procedure
    (lambda (proc1 arg)
      (cases proc proc1
        (procedure (var body saved-env)
	  (let ((r arg))
	    (let ((new-env (extend-env var r saved-env)))
	      (if (instrument-let)
		(begin
		  (eopl:printf
		    "entering body of proc ~s with env =~%"
		    var)
		  (pretty-print (env->list new-env))
                  (eopl:printf "store =~%")
                  (pretty-print (store->readable (get-store-as-list)))
                  (eopl:printf "~%")))
              (value-of body new-env)))))))
  
  
  (define bool?
       (lambda (x)
        (if (or (= x #t) (= x #f))
            (#t)
            (#f))))


  ;; store->readable : Listof(List(Ref,Expval)) 
  ;;                    -> Listof(List(Ref,Something-Readable))
  (define store->readable
    (lambda (l)
      (map
        (lambda (p)
          (cons
            (car p)
            (expval->printable (cadr p))))
        l)))
  
   (define legal-list 
    (lambda (typ values lst)
      (cases type typ
        (type-num()
                 (if (null? values) 
                     (lst)
                     ((if (number? (expval->num (car values)))
                                 (legal-list typ (cdr values) (cons (newref(car values)) lst))
                                 (eopl:error 'legal-list "type mismatch ")))))
        (type-bool()
                  (if (null? values) 
                     (lst)
                     ((if (bool? (expval->bool (car values)))
                                 (legal-list typ (cdr values) (cons (newref(car values)) lst))
                                 (eopl:error 'legal-list "type mismatch ")))))
        
        #|(type-proc()
                  (if (null? values) 
                     (lst)
                     ((cases proc (car values)
                        (procedure (var body saved-env)
                                         (legal-list typ (cdr values) (cons (newref(car values)) lst)))
                 (eopl:error 'legal-list "type mismatch ~s" typ)))))|#
        (else (eopl:error 'legal-list "type mismatch ")))))
      
     #| (cases type typ
        (type-num()
                 (if (not (= (length (filter  (lambda(x) (cases expval x (num-val (num) #t) (else #f))) vals)))   (length vals) )
                 (eopl:error 'legal-list "type mismatch ~s" typ)
                 ))
        (type-bool()
                  (if (not (= (length (filter  (lambda(x) (cases expval x (bool-val (bool)) ) vals)))   (length vals) ))
                 (eopl:error 'legal-list "type mismatch ~s" typ)
                 ))
        (type-proc()
                  (if (not (= (length (filter  (lambda(x) (cases expval x proc x (procedure (var body saved-env)#t) (else #f))) vals)))   (length vals) ))
                 (eopl:error 'legal-list "type mismatch ~s" typ)
                 )
        (else
          (eopl:error 'legal-list "type mismatch ~s" typ)))
      (foldr (lambda (v l) (cons (newref v) l)) '()  vals)))|#
  
  

  (define get-val
    (lambda (arry i)
      (if (= 0 i)
          ((if (pair? arry)
              (car arry)
              (arry)))
           (get-val (cdr arry) (- i 1)))))
        


 
  )
  


  
