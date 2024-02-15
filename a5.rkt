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
       (let ((closure (value-of-ds rator env)))
         (apply-clos-ds closure rand env))))))

(trace value-of-ds)


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
    `(closure ,x , body ,env)))

(define apply-clos-ds
  (λ (closure rand env)
    (match closure
      (`(closure ,x ,body ,closure-env)
       (value-of-ds body (extend-env-ds x rand closure-env))))))