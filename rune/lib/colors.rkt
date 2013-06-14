#lang racket/base
(require racket/class
         racket/contract
         racket/draw
         (for-syntax racket/base
                     racket/syntax
                     syntax/parse))

(define color/c (is-a?/c color%))

(define (color r g b)
  (make-object color% r g b))

(define-syntax (define-colors stx)
  (syntax-parse stx
    [(_ scheme (~seq name:id r:nat g:nat b:nat) ...)
     (with-syntax
         ([(c:name ...)
           (for/list ([n (in-list (syntax->list #'(name ...)))])
             (format-id n "c:~a" n))])
       (syntax/loc stx
         (begin
           (define c:name (color r g b))
           ...
           (define scheme (vector c:name ...)))))]))

(provide
 define-colors
 (contract-out
  [color/c contract?]))
