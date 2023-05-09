(module data-structures (lib "eopl.ss" "eopl")

  ;; data structures for let-lang.

  (provide (all-defined))               ; too many things to list

;;;;;;;;;;;;;;;; expressed values ;;;;;;;;;;;;;;;;

;;; an expressed value is either a number, a boolean or a procval.

  (define-datatype expval expval?
    (num-val
      (value number?))
    (bool-val
      (boolean boolean?))
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

  (define expval-extractor-error
    (lambda (variant value)
      (eopl:error 'expval-extractors "Looking for a ~s, found ~s"
	variant value)))


  
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

;;;;;;;;;;;;;;;; environment structures ;;;;;;;;;;;;;;;;
  (define-datatype environment environment?
    (empty-env-record)
    (extended-env-record
     (syms (list-of symbol?))
     (vals (list-of expval?))
     (env environment?))
    )
  
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
  
  (define apply-env-ref
    (lambda (env search-sym)
      (cases environment env
        (empty-env-record () (eopl:error 'apply-env "no association for symbol ~s" search-sym))
        (extended-env-record (syms vals old_env)
                             (let ((position (env-find-position search-sym syms)))
                               (if (number? position)
                                   (list-ref vals position)
                                   (apply-env-ref old_env search-sym)))))))  
)
