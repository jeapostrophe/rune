#lang racket/base
(require racket/class
         racket/contract
         racket/match
         racket/draw
         (only-in racket/gui/base make-screen-bitmap)
         rune/lib/context
         (for-syntax racket/base
                     racket/syntax
                     syntax/parse)
         opengl/texture)

(define real-color/c
  (vector/c byte? byte? byte?))

(define (real-color r g b)
  (vector r g b))

(struct colors ([tex #:mutable] vec))

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
           (define scheme (colors #f (vector (real-color r g b) ...))))))]))

(define colors/c
  colors?)
(define color/c
  (integer-in 0 15))

(define (colors-ref cs i)
  (vector-ref (colors-vec cs) i))

(define (set-colors-context! cs ctxt)
  (define bm (make-bitmap 16 1))
  (define bm-dc (send bm make-dc))
  (for ([c (in-vector (colors-vec cs))]
        [i (in-naturals)])
    (match-define (vector r g b) c)
    (eprintf "~a,~a -> ~a,~a,~a\n" i 0 r g b)
    (send bm-dc set-pixel i 0 (make-object color% r g b)))
  (ctxt
   (λ ()
     (set-colors-tex! cs (load-texture/bitmap bm)))))

(provide
 define-colors
 (contract-out
  [color/c contract?]
  [colors/c contract?]
  [colors-ref
   (-> colors/c color/c
       real-color/c)]
  [set-colors-context!
   (-> colors/c context/c
       void)]
  [colors-tex
   (-> colors/c
       exact-nonnegative-integer?)]))
