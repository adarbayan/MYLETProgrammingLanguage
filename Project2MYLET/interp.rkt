#lang eopl

;; interpreter for the LET language.  The \commentboxes are the
;; latex code for inserting the rules into the code in the book.
;; These are too complicated to put here, see the text, sorry.

(require "lang.rkt")
(require "data-structures.rkt")
(require "environments.rkt")

(provide value-of-program value-of)

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

;; value-of-program : Program -> ExpVal
;; Page: 71
(define value-of-program 
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
                 (value-of exp1 (init-env))))))

;; value-of : Exp * Env -> ExpVal
;; Page: 71

(define conds-evaluator
  (lambda (conds exps env)
    (if (not (null? conds))
        (if (expval->bool (value-of (car conds) env))
            (list #t (value-of (car exps) env))
            (conds-evaluator (cdr conds) (cdr exps) env))
        (list #f '()))))

(define switch-evaluator
  (lambda (value nums exps env)
    (if (not (null? nums))
        (if (= value (car nums))
            (list #t (value-of (car exps) env))
            (switch-evaluator value (cdr nums) (cdr exps) env))
        (list #f '()))))

(define value-of
  (lambda (exp env)
    (cases expression exp
      
      ;;\commentbox{ (value-of (const-exp \n{}) \r) = \n{}}
      (const-exp (num) (num-val num))
      
      ;;\commentbox{ (value-of (var-exp \x{}) \r) = (apply-env \r \x{})}
      (var-exp (var) (apply-env env var))

      (str-exp (str) (str-val str))

      (op-exp (exp1 exp2 num)
              (let ((val1 (value-of exp1 env))
                    (val2 (value-of exp2 env)))
                (let ((num1 (expval->num val1))
                      (num2 (expval->num val2)))
                  (cond ((= num 1) (num-val (+ num1 num2)))
                        ((= num 2) (num-val (* num1 num2)))
                        ((= num 3) (num-val (/ num1 num2)))
                        (else (num-val (- num1 num2)))))))
                
     
      ;;\commentbox{\zerotestspec}
      (zero?-exp (exp1)
                 (let ((val1 (value-of exp1 env)))
                   (let ((num1 (expval->num val1)))
                     (if (zero? num1)
                         (bool-val #t)
                         (bool-val #f)))))

      ;;\commentbox{\ma{\theletspecsplit}}
      (let-exp (var exp1 body)       
               (let ((val1 (value-of exp1 env)))
                 (value-of body
                           (extend-env var val1 env))))

      (if-exp (exp1 exp2 conds exps exp3)
              (let ((val1 (expval->bool (value-of exp1 env))))
                (cond (val1 (value-of exp2 env))
                      ((not (null? conds)) (let ((conds-result (conds-evaluator conds exps env)))
                                             (if (car conds-result)
                                                 (cadr conds-result)
                                                 (value-of exp3 env)
                                                 )))
                      (else (value-of exp3 env))
                      )))
                    
                    
      (switch-exp (var nums exps default)
                  (let ((value (expval->num (value-of var env))))
                    (let ((switch-result (switch-evaluator value nums exps env)))
                    (if (car switch-result)
                        (cadr switch-result)
                        (value-of default env)))))
        
      )))
