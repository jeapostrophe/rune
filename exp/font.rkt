#lang racket/base
(require data/integer-set
         racket/draw
         racket/runtime-path
         racket/class
         net/url
         racket/port
         racket/gui/base)

(define (string->char-set s)
  (for/fold ([is (make-range)])
      ([c (in-string s)])
    (union is (make-range (char->integer c)))))

(module+ main
  (define size-bm (make-screen-bitmap 200 200))
  (define size-dc (send size-bm make-dc))

  (send size-dc set-font (make-font #:family 'modern))
  (define-values (cw ch _0 _1) (send size-dc get-text-extent " "))

  (define (disp l is)
    (printf "~a ~a\n" l (* cw ch (count is) 1/1024 1/1024)))

  (disp "all unicode"
        (union (make-range 0 55295)
               (make-range 57344 1114111)))

  (define common
    (string->char-set
     (port->string
      (get-pure-port
       (string->url
        "http://rsiqueira.postbit.com/upload/2/posts/unicode.html")))))

  (disp "common" common)

  (define (union* . l)
    (foldl union (make-range) l))

  (define jpn
    (union* (make-range #x3000 #x303f)
            (make-range #x3040 #x309f)
            (make-range #x30a0 #x30ff)
            (make-range #xff00 #xffef)
            (make-range #x4e00 #x9faf)))

  (disp "jpn" jpn)

  (define (draw-chars p is)
    (define isl (foldr cons null is))
    (define k (count is))
    (define side (ceiling (sqrt k)))

    (define-values (cw ch)
      (for/fold ([cw 0] [ch 0])
          ([ci (in-list isl)])
        (define-values (tw th _0 _1)
          (send size-dc get-text-extent (string (integer->char ci))))
        (values (max cw tw)
                (max ch th))))

    (define full-bm
      (make-object bitmap%
       (inexact->exact (* cw side))
       (inexact->exact (* ch side))
       #t
       #f))
    (define full-dc (send full-bm make-dc))
    (send full-dc set-font (make-font #:family 'modern))

    (for ([ci (in-list isl)]
          [idx (in-naturals)])
      (define x (quotient idx side))
      (define y (modulo idx side))
      (define cis (string (integer->char ci)))
      (send full-dc draw-text cis
            (* x cw) (* y ch)))

    (send full-bm save-file p 'png 100))

  (define-runtime-path both.png "both.png")
  (draw-chars both.png (union common jpn))
  (define-runtime-path font.png "font.png")
  (draw-chars font.png common))
