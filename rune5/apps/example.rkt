#lang racket/base
(require rune/app)

(define-syntax-rule
  (define-singleton id sid
    ([field-id field-exp] ...)
    struct-opt ...)
  (begin (serializable-struct sid (field-id ...) struct-opt ...)
         (define id (sid field-exp ...))))

;; xxx better macro
(define-singleton the-app example
  ([n 0])
  #:methods gen:app
  [(define (app-serialize a) a)
   (define (app-commands a) '(add1 sub1))
   (define (app-command-desc a c)
     (match c
       ['add1 empty]
       ['sub1 empty]
       [_ #f]))   
   (define (app-exec a c)
     (match c
       ['add1 (example ())]))])

(provide the-app)
