#lang racket

;Problem 1
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
      
      (`,y #:when (symbol? y) (env y))
      
      (`(let ((,x ,v)) ,body)
       #:when (symbol? x)
       (let ((val (value-of-ds v env)))
         (value-of-ds body (λ (y)
                             (cond
                               ((eqv? y x) val)
                               (else (env y)))))))
      
      (`(λ (,x) ,body)
       #:when (symbol? x)
       ;; Replacing the formal env with env^ turns this into
       ;; a lexically scoped interpreter
       (λ (arg env)
         (value-of-ds body (λ (y)
                             (cond
                               ((eqv? y x) arg)
                               (else (env y)))))))
      
      (`(,rator ,rand)
       ((value-of-ds rator env) (value-of-ds rand env) env)))))




