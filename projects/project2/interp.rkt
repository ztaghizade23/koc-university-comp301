#lang eopl

;; interpreter for the PROC language, using the procedural
;; representation of procedures.



(require "lang.rkt")
(require "data-structures.rkt")
(require "environments.rkt")

(provide value-of-program value-of)

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

;; run : String -> ExpVal
(define run
  (lambda (s)
    (value-of-program (scan&parse s))))

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
      
      
      (const-exp (num) (num-val num))
      
      
      (var-exp (var) (apply-env env var))
      
     
      (diff-exp (exp1 exp2)
                (let ((val1 (value-of exp1 env))
                      (val2 (value-of exp2 env)))
                  (let ((num1 (expval->num val1))
                        (num2 (expval->num val2)))
                    (num-val
                     (- num1 num2)))))
      
      
      (zero?-exp (exp1)
                (let ((val1 (value-of exp1 env)))
                  (let ((num1 (expval->num val1)))
                    (if (zero? num1)
                        (bool-val #t)
                        (bool-val #f)))))
      
     
      (if-exp (exp1 exp2 exp3)
                (let ((val1 (value-of exp1 env)))
                  (if (expval->bool val1)
                    (value-of exp2 env)
                    (value-of exp3 env))))
      
      
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
      
      ;;----------------------------------------------------
      ; INSERT YOUR CODE HERE
      ; Write the required expressions starting from here
      
      (stack-exp ()
                (stack-val '()))

      (stack-push-exp (stack exp)
                (let ((stack (expval->stack (value-of stack env)))
                      (exp (expval->num (value-of exp env))))
                  (stack-val (cons exp stack))))

      (stack-pop-exp (exp)
                (let ((stack (expval->stack (value-of exp env))))
                  (if (null? stack) (begin
                        (eopl:printf "WARNING ~n  stack-pop-exp: Stack is empty~n~n")
                        (stack-val '()))
                      (stack-val (cdr stack)))))

      (stack-peek-exp (exp)
                (let ((stack (expval->stack (value-of exp env))))
                  (if (null? stack) (begin
                        (eopl:printf "WARNING ~n  stack-peek-exp: Stack is empty~n~n")
                        (num-val 2813))
                      (num-val (car stack)))))
      
      (stack-push-multi-exp (stack exps)
                (let ((stack (expval->stack (value-of stack env)))
                      (exps (map (lambda (exp) (expval->num (value-of exp env))) exps)))
                  (stack-val (stack-multipush-primitive stack exps))))

      (stack-pop-multi-exp (stack num)
                (let ((stack (expval->stack (value-of stack env)))
                      (num (expval->num (value-of num env))))
                  (if (< (length stack) num) (begin
                        (eopl:printf "WARNING ~n  stack-pop-multi-exp: n = ~s is larger than the length of stack ~s~n~n" num stack)
                        (stack-val '()))
                      (stack-val (stack-multipop-primitive stack num)))))

      
      (stack-merge-exp (stack1 stack2)
                (let ((stack1 (expval->stack (value-of stack1 env)))
                      (stack2 (expval->stack (value-of stack2 env))))
                  (stack-val (stack-multipush-primitive stack1 stack2))))
      ;;-------------------------------------------------
      
      )))

;;-----------------------------------------
; INSERT YOUR CODE HERE
; you may use this area to define helper functions
;;-----------------------------------------

(define (stack-multipush-primitive stack-list values-list)
  (if (null? values-list)
     stack-list
   (stack-multipush-primitive
     (cons (car values-list) stack-list)
     (cdr values-list))))


(define (stack-multipop-primitive stack-list n)
  (if (<= n 0)
     stack-list
   (stack-multipop-primitive
     (cdr stack-list)
     (- n 1))))

;;-----------------------------------------

(define apply-procedure
  (lambda (proc1 val)
    (cases proc proc1
      (procedure (var body saved-env)
                 (value-of body (extend-env var val saved-env))))))
