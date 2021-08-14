#lang racket

(provide (all-defined-out))

;;constructor
;;Binding pt2
(define (make-binding v1 v2)
  (list v1 v2))

(define (binding-variable b)
  (car b))
(define (binding-value b)
  (cadr b))

;;Frames pt3
;Constructor
(define (make-frame vars vals)
  (define (m-f vars vals)
    (cond [(empty? vars) '()]
          [else (cons (make-binding (car vars) (car vals))
                      (m-f (cdr vars) (cdr vals)))]))
  (if (not (eq? (length vars) (length vals)))
      (println "Error: lists are not the same length!")
      (m-f vars vals)))

;(make-frame '(a b c) '(1 2 3))
;;empty-frame?
(define (empty-frame? frame)
  (empty? frame))
;;first-binding
(define (first-binding frame)
  (car frame))
;;rest-binding
(define (rest-of-binding frame)
  (cdr frame))

(define (adjoin-binding binding frame)
  (cons binding frame))

(define (binding-in-frame var frame)
  (cond [(empty-frame? frame) #f]
        [(equal? (binding-variable (first-binding frame)) var) (first-binding frame)]
        [else (binding-in-frame var (rest-of-binding frame))]))

;;pt4 Environment

;;constructor
(define (empty-env)
  (list ))

(define (empty-env? env)
  (empty? env))

(define (first-frame env)
  (mcar env))

(define (rest-of-frames env)
  (mcdr env))

(define (set-first-frame! env new-frame)
  (set-mcar! env new-frame))

(define (adjoin-frame frame env)
  (mcons frame env))

(define (extend-env vars vals base-env)
  (adjoin-frame (make-frame vars vals) base-env))

(define (binding-in-env var env)
  (cond [(empty-env? env) #f]
        [else (let ((found (binding-in-frame var (first-frame env))))
                (if found
                    found
                    (binding-in-env var (rest-of-frames env))))]))

(define (lookup-variable var env)
  (let ((found (binding-in-env var env)))
    (if found
        (binding-value found)
        (println "Error: variable not bound in environment!"))))

;;pt5

(define (setup-env)
  (extend-env '(null) '(()) (empty-env)))

(define global-env (setup-env))

;;pt6.1
#|
(define (i-eval exp env)
  (cond [(boolean? exp) exp]
        [(number? exp) exp]
        [(string? exp) exp]
        (println "Error: unknown expression type")))
|#
;;Pt6.2
(define (read-eval-print-loop)
  (display "INTERPRETER> ")
  (let ((user-input (read)))
    (display (i-eval user-input global-env))
    (newline)
    (read-eval-print-loop)))
(define (repl)
  (read-eval-print-loop))

;;Pt6.3
#|
(define (quoted? input)
  (cond [(not (list? exp)) #f]
        [(> (length exp) 1) (equal? (car exp) 'quote)]))
|#
(define (quoted? input)
  (tagged-list? input 'quote))

(define (text-of-quotation input)
  (cadr input))
#|
(define (i-eval exp env)
  (cond [(quoted? exp) (text-of-quotation exp)]
        [(boolean? exp) exp]
        [(number? exp) exp]
        [(string? exp) exp]
        (println "Error: unknown expression type")))
|#

;;pt6.4

(define (tagged-list? exp tag)
  (cond [(not (list? exp)) #f]
        [(> (length exp) 1) (equal? (car exp) tag)]))

(define (definition? exp)
  (tagged-list? exp 'define))
              
(define (definition-variable exp)
  (cadr exp))
(define (definition-value exp)
  (caddr exp))

(define (eval-definition exp env)
  (let ((variable (definition-variable exp))
        (value (i-eval (definition-value exp) env)))
    (define-variable! variable value env)))

(define (define-variable! var val env)
  (if (binding-in-frame var (first-frame env))
      (println "Error: Duplicate definition for identifier")
      (set-first-frame! env (adjoin-binding (make-binding var val) (first-frame env)))))

(define (variable? exp)
  (symbol? exp))


(define (i-eval exp env)
  (cond [(equal? exp 'env) env]
        [(definition? exp) (eval-definition exp env)]
        [(quoted? exp) (text-of-quotation exp)]
        [(variable? exp)(lookup-variable exp env)]
        [(boolean? exp) exp]
        [(number? exp) exp]
        [(string? exp) exp]
        (println "Error: unknown expression type")))

(define (tagged-list-length-n? exp tag n)
  (and (tagged-list-length-n? exp tag)
      (eq? (length exp) n)))

(define (tagged-list-min-length-n? exp tag n)
  (and (tagged-list-length-n? exp tag)
      (>= (length exp) n)))


;(repl)
      
        





  

  




