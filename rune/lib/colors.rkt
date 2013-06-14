#lang racket/base
(require racket/class
         racket/contract
         racket/draw
         rune/lib/context
         (for-syntax racket/base
                     racket/syntax
                     syntax/parse))

(define real-color/c (is-a?/c color%))

(define (color r g b)
  (make-object color% r g b))

(define-syntax (define-colors stx)
  (syntax-parse stx
    [(_ scheme (~seq name:id r:nat g:nat b:nat) ...)
     (with-syntax
         ([((c:name c:name-i) ...)
           (for/list ([n (in-list (syntax->list #'(name ...)))]
                      [i (in-naturals)])
             (list (format-id n "c:~a" n) i))])
       (syntax/loc stx
         (begin
           (define c:name c:name-i)
           ...
           (define scheme (vector (color r g b) ...)))))]))

(define colors/c
  (apply vector/c (build-list 16 (λ (i) real-color/c))))
(define color/c
  (integer-in 0 15))

;; OpenGL: Turn colors-ref into the identity function/macro.
(define colors-ref vector-ref)

;; OpenGL: Use the context to load the colors into a texture
;; and provide access to what it is
(define (set-colors-context! cs ctxt)
  (void))

(provide
 define-colors
 (contract-out
  [color/c contract?]
  [colors/c contract?]
  [set-colors-context!
   (-> colors/c context/c
       void)]
  [colors-ref
   (-> colors/c color/c
       real-color/c)]))
