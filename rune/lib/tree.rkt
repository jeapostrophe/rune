#lang racket/base
(require racket/match
         racket/contract)

;; xxx
(define (tree/c ?)
  any/c)

(define (tree-iter! f t)
  (match t
    [(or #f (? void?) (list))
     0]
    [(cons a d)
     (+ (tree-iter! f a)
        (tree-iter! f d))]
    [x
     (f x)
     1]))

(provide tree/c
         tree-iter!)
