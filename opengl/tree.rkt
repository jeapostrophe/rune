#lang racket/base
(require racket/match
         racket/contract)

;; xxx
(define (tree/c ?)
  any/c)

(define (tree-iter!/k k f t)
  (match t
    [(or #f (? void?) (list))
     k]
    [(cons a d)
     (tree-iter!/k (tree-iter!/k k f a) f d)]
    [x
     (f k x)
     (add1 k)]))

(define (tree-iter! f t)
  (tree-iter!/k 0 f t))

(define (tree-count t)
  (tree-iter! void t))

(provide tree/c
         tree-iter!
         tree-count)