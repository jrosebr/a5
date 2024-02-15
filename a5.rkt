#lang racket

;Problem 1

(require racket/trace)

(define value-of-ds
  (λ (e env)
    (match e
      (`,n #:when (number? n) n)
      
      (`(+ ,nexpr1 ,nexpr2)
       (+ (value-of-ds nexpr1 env)
          (value-of-ds nexpr2 env)))
      
      (`(if-null ,l ,null-expr ,not-null-expr)
       (if (null? (value-of-ds l env))
           (value-of-ds null-expr env)
           (value-of-ds not-null-expr env)))
      
      (`(cons ,car-expr ,cdr-expr)
       (cons (value-of-ds car-expr env)
             (value-of-ds cdr-expr env)))
      
      (`(car ,l-expr)
       (car (value-of-ds l-expr env)))
      
      (`(cdr ,l-expr)
       (cdr (value-of-ds l-expr env)))
      
      (`empty '())
      
      (`,y #:when (symbol? y) (apply-env-ds env y))
      
      (`(let ((,x ,v)) ,body)
       #:when (symbol? x)
       (let ((val (value-of-ds v env)))
         (value-of-ds body (extend-env-ds x val env))))
      
      (`(λ (,x) ,body)
       #:when (symbol? x)
       ;; Replacing the formal env with env^ turns this into
       ;; a lexically scoped interpreter
       `(closure ,x ,body ,env))
      
      (`(,rator ,rand)
       (let ((closure (value-of-ds rator env))
             (arg (value-of-ds rand env)))
         (apply-clos-ds closure arg env))))))


(define empty-env-ds
  (λ ()
    '()))

(define extend-env-ds
  (λ (x arg env)
    (cons (cons x arg) env)))

(define apply-env-ds
  (λ (env y)
    (let ((binding (assoc y env)))
      (if binding
          (cdr binding)
          (error 'apply-env-ds "unbound variable: ~a" y)))))

(define make-clos-ds
  (λ (x body env)
    (λ (arg)
      ; We also need to extend the environment
      (value-of-ds body (extend-env-ds x arg env)))))

(define apply-clos-ds
  (λ (closure rand env)
    (match closure
      (`(closure ,x ,body ,closure-env)
       (value-of-ds body (extend-env-ds x rand env))))))

;Problem 2

;val-of-cbv
(define val-of-cbv
  (λ (e env)
    (match e
      (`,y #:when (symbol? y) (unbox (env y)))
      
      (`,b #:when (boolean? b) b)
      
      (`,n #:when (number? n) n)

      (`(zero? ,n) (zero? (val-of-cbv n env)))
      
      (`(* ,nexpr1 ,nexpr2)
       (* (val-of-cbv nexpr1 env)
          (val-of-cbv nexpr2 env)))

      (`(sub1 ,n) (sub1 (val-of-cbv n env)))

      (`(if ,test ,conseq ,alt) (if (val-of-cbv test env)
                                    (val-of-cbv (conseq env))
                                    (val-of-cbv (alt env))))
      
      (`(random ,n) (random (val-of-cbv n env)))
      
      (`(begin2 ,exp1 ,exp2)
       (begin
         (val-of-cbv exp1 env)
         (val-of-cbv exp2 env)))
      
      (`(set! ,x ,expr)
       #:when (symbol? x)
       (set-box! (env x) (val-of-cbv expr env))) 
      
      (`(lambda (,x) ,body)
       #:when (symbol? x)
       (make-clos-cbv x body env))
      
      (`(,rator ,rand)
       (let ((closure (val-of-cbv rator env))
             (arg (box (val-of-cbv rand env))))
         (apply-clos-cbv closure arg))))))

(define empty-env
  (λ ()
    (λ (y)
      (error 'val-of-cbv "unbound ~a" y))))

(define extend-env-cbv
  (λ (x arg env)
    (λ (y)
      (cond
        ((eqv? y x) arg)
        (else (apply-env-cbv env y))))))

(define apply-env-cbv
  (λ (env y)
    (env y)))

(define make-clos-cbv
  (λ (x body env)
    (λ (arg)
      ; We also need to extend the environment
      (val-of-cbv body (extend-env-cbv x arg env)))))

(define apply-clos-cbv
  (λ (clos arg)
    (clos arg)))

;test cases
#|
(val-of-cbv
   '((lambda (a)
       ((lambda (p)
          (begin2
           (p a)
           a)) (lambda (x) (set! x 4)))) 3)
   (empty-env))

 (val-of-cbv
   '((lambda (f)
       ((lambda (g)
          ((lambda (z) (begin2
                        (g z)
                        z))
           55))
        (lambda (y) (f y)))) (lambda (x) (set! x 44)))
   (empty-env))

|#

;val-of-cbr
(define val-of-cbr
  (λ (e env)
    (match e
      (`,y #:when (symbol? y) (unbox (env y)))
      
      (`,n #:when (number? n) n)

      (`,b #:when (boolean? b) b)

      (`(zero? ,n) (zero? (val-of-cbr n env)))

      (`(sub1 ,n) (sub1 (val-of-cbr n env)))
      
      (`(* ,nexpr1 ,nexpr2)
       (* (val-of-cbr nexpr1 env)
          (val-of-cbr nexpr2 env)))

      (`(if ,test ,conseq ,alt) (if (val-of-cbr test env)
                                    (val-of-cbr conseq env)
                                    (val-of-cbr alt env)))
      
      (`(begin2 ,exp1 ,exp2)
       (begin
         (val-of-cbr exp1 env)
         (val-of-cbr exp2 env)))
      
      (`(set! ,x ,expr)
       #:when (symbol? x)
       (set-box! (env x) (val-of-cbr expr env)))

      (`(random ,n) (random (val-of-cbr n env)))
      
      (`(lambda (,x) ,body)
       #:when (symbol? x)
       (λ (arg)
         ; We also need to extend the environment
         (val-of-cbr body (extend-env-cbr x arg env))))
      
      (`(,rator ,x)
       #:when (symbol? x)
       ((val-of-cbr rator env) (env x)))
      
      (`(,rator ,rand)
       (let ((closure (val-of-cbr rator env))
             (arg (box (val-of-cbr rand env))))
         (apply-clos-cbr closure arg))))))

(define empty-env-cbr
  (λ ()
    (λ (y)
      (error 'val-of-cbr "unbound ~a" y))))

(define extend-env-cbr
  (λ (x arg env)
    (λ (y)
      (cond
        ((eqv? y x) arg)
        (else (apply-env-cbr env y))))))

(define apply-env-cbr
  (λ (env y)
    (env y)))

(define make-clos-cbr
  (λ (x body env)
    (λ (arg)
      ; We also need to extend the environment
      (val-of-cbr body (extend-env-cbr x arg env)))))

(define apply-clos-cbr
  (λ (clos arg)
    (clos arg)))
 

;test cases
#|
(val-of-cbr
   '((lambda (x) (begin2 (set! x #t)
                         (if x 3 5))) #f)
   (empty-env))

(val-of-cbr
   '((lambda (a)
       ((lambda (p)
          (begin2
           (p a)
           a)) (lambda (x) (set! x 4)))) 3)
   (empty-env))

(val-of-cbr
   '((lambda (f)
       ((lambda (g)
          ((lambda (z) (begin2
                        (g z)
                        z))
           55))
        (lambda (y) (f y)))) (lambda (x) (set! x 44)))
   (empty-env))
|#


;val-of-cbname
(define random-sieve
    '((lambda (n)
        (if (zero? n)
            (if (zero? n) (if (zero? n) (if (zero? n) (if (zero? n) (if (zero? n) (if (zero? n) #t #f) #f) #f) #f) #f) #f)
            (if (zero? n) #f (if (zero? n) #f (if (zero? n) #f (if (zero? n) #f (if (zero? n) #f (if (zero? n) #f #t))))))))
      (random 2)))


(define val-of-cbname
  (λ (e env)
    (match e
      (`,y #:when (symbol? y) ((unbox (env y))))
      
      (`,n #:when (number? n) n)

      (`,n #:when (boolean? n) n)
      
      (`(* ,nexpr1 ,nexpr2)
       (* (val-of-cbname nexpr1 env)
          (val-of-cbname nexpr2 env)))

      (`(zero? ,n) (zero? (val-of-cbname n env)))

      (`(sub1 ,n) (sub1 (val-of-cbname n env)))

      (`(if ,test ,conseq ,alt) (if (val-of-cbname test env)
                                    (val-of-cbname conseq env)
                                    (val-of-cbname alt env)))

      (`(random ,n) (random(val-of-cbname n env)))
      
      (`(lambda (,x) ,body)
       #:when (symbol? x)
       (λ (arg)
         ; We also need to extend the environment
         (val-of-cbname body (extend-env-cbname x arg env))))
      
      (`(,rator ,x)
       #:when (symbol? x)
       ((val-of-cbname rator env) (env x)))
      
      (`(,rator ,rand)
       ((val-of-cbname rator env)
        (box (λ ()
               ;(printf "Expensive computation!!~n")
               (val-of-cbname rand env))))))))

(define empty-env-cbname
  (λ ()
    (λ (y)
      (error 'val-of-cbname "unbound ~a" y))))

(define extend-env-cbname
  (λ (x arg env)
    (λ (y)
      (cond
        ((eqv? y x) arg)
        (else (apply-env-cbname env y))))))

(define apply-env-cbname
  (λ (env y)
    (env y)))

(define make-clos-cbname
  (λ (x body env)
    (λ (arg)
      ; We also need to extend the environment
      (val-of-cbname body (extend-env-cbname x arg env)))))

(define apply-clos-cbname
  (λ (clos arg)
    (clos arg)))

;test cases
#|
(val-of-cbname random-sieve (empty-env-cbname))
(val-of-cbname
   '((lambda (z) 100)
     ((lambda (x) (x x)) (lambda (x) (x x))))
   (empty-env))
|#


;val-of-cbneed
(define val-of-cbneed
  (λ (e env)
    (match e
      (`,y
       #:when (symbol? y)
       (let ((b (env y)))
         (let ((th (unbox b)))
           (let ((v (th)))
             (begin
               (set-box! b (λ () v))
               v)))))
      
      (`,n #:when (number? n) n)

      (`,b #:when (boolean? b) b)

      (`(zero? ,n) (zero? (val-of-cbneed n env)))

      (`(sub1 , n) (sub1 (val-of-cbneed n env)))

      (`(if ,test ,conseq ,alt) (if (val-of-cbneed test env)
                                    (val-of-cbneed conseq env)
                                    (val-of-cbneed alt env)))

      (`(random ,n) (random (val-of-cbneed n env)))
      
      (`(* ,nexpr1 ,nexpr2)
       (* (val-of-cbneed nexpr1 env)
          (val-of-cbneed nexpr2 env)))
      
      (`(lambda (,x) ,body)
       #:when (symbol? x)
       (λ (arg)
         ; We also need to extend the environment
         (val-of-cbneed body (extend-env-cbneed x arg env))))
      
      (`(,rator ,x)
       #:when (symbol? x)
       ((val-of-cbneed rator env) (env x)))
      
      (`(,rator ,rand)
       ((val-of-cbneed rator env)
        (box (λ ()
               (printf "Expensive computation!!~n")
               (val-of-cbneed rand env))))))))

;(trace val-of-cbneed)

(define empty-env-cbneed
  (λ ()
    (λ (y)
      (error 'val-of-cbneed "unbound ~a" y))))

(define extend-env-cbneed
  (λ (x arg env)
    (λ (y)
      (cond
        ((eqv? y x) arg)
        (else (apply-env-cbneed env y))))))

(define apply-env-cbneed
  (λ (env y)
    (env y)))

(define make-clos-cbneed
  (λ (x body env)
    (λ (arg)
      ; We also need to extend the environment
      (val-of-cbneed body (extend-env-cbneed x arg env)))))

(define apply-clos-cbneed
  (λ (clos arg)
    (clos arg)))

;test case
;(val-of-cbneed random-sieve (empty-env))