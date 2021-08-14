#lang racket

(require "interpreter.rkt")
(require rackunit)
;;Pt 2

(define sample (make-binding 'today 'monday))
(define sample2 (make-binding 'a '(b c)))
(define sample3 (make-binding 'apple 'bananna))
(check-equal? sample '(today monday))
(check-equal? sample2 '(a (b c)))
(check-equal? sample3 '(apple bananna))

;(binding-variable sample)
(check-equal? (binding-variable sample) 'today)
(check-equal? (binding-variable sample2) 'a)
(check-equal? (binding-variable sample3) 'apple)

;(binding-value sample)
(check-equal? (binding-value sample) 'monday)
(check-equal? (binding-value sample2) '(b c))
(check-equal? (binding-value sample3) 'bananna)


;;Pt3 (WRITE MORE)
;make-frame
(define frame (make-frame '(a b c) '(6 7 8)))
(define frame2 (make-frame '(d e) '((1 2) (3 4))))
(check-equal? '((a 6) (b 7) (c 8)) frame)
(check-equal? '((d (1 2)) (e (3 4))) frame2)
;empty-frame
(check-true (empty-frame? (make-frame '() '())))
(check-false (empty-frame? frame))
;first-binding
(check-equal? (first-binding frame) '(a 6))
;rest-of-binding
(check-equal? (rest-of-binding frame) '((b 7) (c 8)))
;adjoin-binding
(check-equal? (adjoin-binding (make-binding 'd '(1 2)) (make-frame '(e) '((3 4)))) frame2)
;binding-in-frame
(check-equal? (binding-in-frame 'd frame2) '(d (1 2)))
(check-false (binding-in-frame 'f frame2))



;;pt4
(define env1 (extend-env '(a b c) '(1 2 3) (empty-env)))
(define env2 (extend-env '(a c d e) '(red blue green yellow) env1))
(define env3 (extend-env '(a f) '(#t #f) env2))

;empty-env
(check-equal? (empty-env) '())
;empty-env?
(check-true (empty-env? (empty-env)))
(check-false (empty-env? env1))
;first-frames
(check-equal? (first-frame env3) '((a #t) (f #f)))
;rest-of-frames
(check-equal? (rest-of-frames env3)
              (mcons '((a red) (c blue) (d green) (e yellow)) (mcons '((a 1) (b 2) (c 3)) '())))
;set-first-frame!
(set-first-frame! env1 (make-frame '(something) '(new))) 
;env1
(check-equal? env1 (mcons '((something new)) '()))
;adjoin-frame
(check-equal? (adjoin-frame frame env1) (mcons '((a 6) (b 7) (c 8))
                                               (mcons '((something new)) '())))
;exend-env
;Kind of tested above earlier as it was the defined env
(check-equal? (extend-env '(a b) '(9 8) (empty-env)) (mcons '((a 9) (b 8)) '()))

;binding in env
(check-equal? (binding-in-env 'something env1) '(something new))
(check-false (binding-in-env 'new env1))

;setup-env
(check-equal? (setup-env) (mcons '((null ())) '()))


;;pt5
(check-equal? (lookup-variable 'something env1) 'new)
;(check-false (lookup-variable 'new env1)) THIS PRINTS AN ERROR CAN'T CHECK
;variable
(check-true (variable? 'x))
(check-false (variable? #f))

;pt6
(define list-tag1 '(define x 5))
(define list-tag2 '(quote x))

(check-true (tagged-list? list-tag1 'define))
(check-false (tagged-list? list-tag1 'quote))
(check-false (tagged-list? list-tag2 'define))
(check-true (tagged-list? list-tag2 'quote))

;definition
(check-true (definition? list-tag1))
(check-false (definition? list-tag2))

;eval-definition changes environment not easily testable
;with lines of code works in the interpreter

;(definition-variable
(check-equal? (definition-variable list-tag1) 'x)
(check-equal? (definition-value list-tag1) 5)

;define-variable! not easy to test either
(define-variable! 'x 5 env1)
(check-equal? env1 (mcons '((x 5) (something new)) '()))
#|
(define (i-eval exp env)
  (cond [(equal? exp 'env) env]
        [(definition? exp) (eval-definition exp env)]
        [(quoted? exp) (text-of-quotation exp)]
        [(variable? exp)(lookup-variable exp env)]
        [(boolean? exp) exp]
        [(number? exp) exp]
        [(string? exp) exp]
        (println "Error: unknown expression type")))
|#
;;I-EVAL
(check-equal? (i-eval #t env1) #t)
(check-equal? (i-eval (quote 'x) env1) 'x)
(check-equal? (i-eval 5 env1) 5)
(check-equal? (i-eval "test" env1) "test")
(check-equal? (i-eval 'x env1) 5)







