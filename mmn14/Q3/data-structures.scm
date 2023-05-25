(module data-structures (lib "eopl.ss" "eopl")

  ;; data structures for proc-lang/ds-rep

  (require "lang.scm")                  ; for expression?

  (provide (all-defined))               ; too many things to list

;;;;;;;;;;;;;;;; expressed values ;;;;;;;;;;;;;;;;

;;; an expressed value is either a number, a boolean or a procval.

  (define-datatype expval expval?
    (num-val
      (value number?))
    (bool-val
      (boolean boolean?))
    (proc-val 
      (proc proc?))  
    ;;expressed value for empty list
    (emptylist-val)
    ;;expressed value for cons
    (cons-val
     (first expval?) (rest expval?)))

;;; extractors:

  ;; expval->num : ExpVal -> Int
  (define expval->num
    (lambda (v)
      (cases expval v
	(num-val (num) num)
	(else (expval-extractor-error 'num v)))))

  ;; expval->bool : ExpVal -> Bool
  (define expval->bool
    (lambda (v)
      (cases expval v
	(bool-val (bool) bool)
	(else (expval-extractor-error 'bool v)))))

  ;; expval->proc : ExpVal -> Proc
  (define expval->proc
    (lambda (v)
      (cases expval v
	(proc-val (proc) proc)
	(else (expval-extractor-error 'proc v)))))

  (define expval-extractor-error
    (lambda (variant value)
      (eopl:error 'expval-extractors "Looking for a ~s, found ~s"
	variant value)))
  
  
  ;;;;;;;;;;;;;;;;Q3;;;;;;;;;;;;;;;;;;;;;;;
    
  ;; expval->car : ExpVal -> ExpVal
  (define expval->car
    (lambda (v)
      (cases expval v
	(cons-val (first rest) first)
	(else (expval-extractor-error 'car v)))))
  
  ;; expval->cdr : ExpVal -> ExpVal
  (define expval->cdr
    (lambda (v)
      (cases expval v
	(cons-val (first rest) rest)
	(else (expval-extractor-error 'cdr v)))))
  
  ;; expval->null? : ExpVal -> bool
  (define expval->null?
    (lambda (v)
      (cases expval v
   ;; Real empty list
	(emptylist-val () #t)
   ;; A list but not an empty one
   (cons-val (first rest) #f)  ;cons
   (num-val (num) #f)   ;number
   (bool-val (bool) #f)     ;boolean
	(else (expval-extractor-error null? v)))))

;;;;;;;;;;;;;;;; procedures ;;;;;;;;;;;;;;;;

  ;; proc? : SchemeVal -> Bool
  ;; procedure : Var * Exp * Env -> Proc
  (define-datatype proc proc?
    (procedure
      (vars list?)
      (exps list?)
      (body expression?)
      (env environment?)))

;;;;;;;;;;;;;;;; environment structures ;;;;;;;;;;;;;;;;
  
  
    (define env-find-position 
    (lambda (sym los)
      (list-find-position sym los)))
  
  (define list-find-position
    (lambda (sym los)
      (list-index (lambda (sym1) (eqv? sym1 sym)) los)))
  
  (define list-index
    (lambda (pred ls)
      (cond
        ((null? ls) #f)
        ((pred (car ls)) 0)
        (else (let ((list-index-r (list-index pred (cdr ls))))
                (if (number? list-index-r)
                    (+ list-index-r 1)
                    #f))))))

;; example of a data type built without define-datatype

  (define empty-env-record
    (lambda () 
      '()))

  (define extended-env-record
    (lambda (sym val old-env)
      (cons (list sym val) old-env)))
  
  (define empty-env-record? null?)
  
  (define environment?
    (lambda (x)
      (or (empty-env-record? x)
          (and (pair? x)
               (symbol? (car (car x)))
               (expval? (cadr (car x)))
               (environment? (cdr x))))))

  (define extended-env-record->sym
    (lambda (r)
      (car (car r))))

  (define extended-env-record->val
    (lambda (r)
      (cadr (car r))))

  (define extended-env-record->old-env
    (lambda (r)
      (cdr r)))

)