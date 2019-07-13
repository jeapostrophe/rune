#lang racket/base
(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse)
         syntax/parse/define
         racket/match
         racket/class
         racket/draw)

;; Colors & Fonts
(define-simple-macro
  (define-colors (~seq c:id r:nat g:nat b:nat) ...)
  #:with (c-c ...) (for/list ([c (in-list (syntax->list #'(c ...)))])
                     (format-id c "c:~a" c))
  (begin (define c-c (make-object color% r g b)) ...))
(define-colors
  ui-lo     #x00 #x2b #x36
  ui-mi     #x07 #x36 #x42
  fg-hi     #x58 #x6e #x75
  fg-mi     #x65 #x7b #x83
  ui-hi     #x83 #x94 #x96
  fg-lo     #x93 #xa1 #xa1
  bg-hi     #xee #xe8 #xd5
  bg        #xfd #xf6 #xe3
  yellow    #xb5 #x89 #x00
  orange    #xcb #x4b #x16
  red       #xdc #x32 #x2f
  magenta   #xd3 #x36 #x82
  violet    #x6c #x71 #xc4
  blue      #x26 #x8b #xd2
  cyan      #x2a #xa1 #x98
  green     #x85 #x99 #x00)

(define default-font "Triplicate T4c")

;; XXX A document describes the document, but we need two separate
;; structures. One is the render-tree that contains the layout
;; commands. One is the "focus" which captures what the browser is
;; actively displaying, and what it isn't.

;; Documents
(struct doc (w h))
(struct doc:empty doc ())
(struct doc:text doc (t)) ;; XXX
(struct doc:hori doc (l r))
(struct doc:vert doc (u d))
(struct doc:frame doc (i))

(define mt (doc:empty 0 0))
(define (text s)
  (doc:text (string-length s) 1 s))
(define (hori2 x y)
  (match-define (doc wx hx) x)
  (match-define (doc wy hy) y)
  (doc:hori (+ (add1 wx) wy) (max hx hy) x y))
(define (vert2 x y)
  (match-define (doc wx hx) x)
  (match-define (doc wy hy) y)
  (doc:vert (max wx wy) (+ (add1 hx) hy) x y))
(define (frame i)
  (match-define (doc wi hi) i)
  (doc:frame (add1 wi) (add1 hi) i))

(define (nary f)
  (define (rec args)
    (match args
      ['() mt]
      [(list x) x]
      [(list x y) (f x y)]
      [(cons x more) (f x (rec more))]))
  (λ args (rec args)))
(define hori (nary hori2))
(define vert (nary vert2))
(define (enumerate #:ordered? [ordered? #f]
                   #:bullet [bullet (text "•")]
                   docs)
  (apply vert
         (if ordered?
           (for/list ([d (in-list docs)] [i (in-naturals)])
             (hori (text (format "~a." i)) d))
           (for/list ([d (in-list docs)])
             (hori bullet d)))))

(define (draw-doc wpc hpc wx hx dc x y d)
  (define (rec x y d) (draw-doc wpc hpc wx hx dc x y d))
  (when
      ;; XXX generalize to bounding box test
      (and (<= 0 x wx)
           (<= 0 y hx))
    (match d
      [(doc:empty _ _)
       (void)]
      [(doc:text _ _ s)
       (send dc draw-text s x y)]
      [(doc:hori _ _ (and (doc lwc _) l) r)
       (rec x y l)
       (define lw (* wpc (add1 lwc)))
       (rec (+ x lw) y r)]
      [(doc:vert _ _ (and (doc _ uhc) u) d)
       (rec x y u)
       (define uh (* hpc (add1 uhc)))
       (rec x (+ y uh) d)]
      [(doc:frame _ _ (and (doc iwc ihc) i))
       (send dc draw-rectangle x y (* wpc (add1 iwc)) (* hpc (add1 ihc)))
       (rec (+ x (* 0.5 wpc)) (+ y (* 0.5 hpc)) i)])))

;; Browser
(require lux lux/chaos/gui)
(define base-browse (word #:fps 0.0 #:label "Rune"))
(define (browse-word d)
  (word/rec
   me base-browse
   #:event (λ (e)
             (cond
               [(eq? e 'close) #f]
               [else me]))
   #:output (λ (w h dc)
              (send dc set-background c:bg)
              (send dc clear)

              (define the-font
                (make-font #:face default-font
                           #:size 13.0))
              (define-values (wpc hpc)
                (let ()
                  (define-values
                      (tw th _b _x)
                    (send dc get-text-extent (make-string 80 #\0) the-font))
                  (values (/ tw 80) th)))

              (send dc set-font the-font)
              (send dc set-text-foreground c:fg-mi)
              (send dc set-pen c:ui-mi 1 'solid)
              (send dc set-brush c:bg 'solid)

              (define border-wx 5)
              (define border-hx 5)

              (draw-doc wpc hpc w h dc border-wx border-hx d))))
(define (browse d)
  (call-with-chaos
   (make-gui
    #:frame-style '(no-caption)
    #:mode 'draw)
   (λ ()
     (fiat-lux (browse-word d)))))

;; Application
(module+ main
  (define x (frame (text "This is a test!")))
  (browse
   (vert
    (hori x
          (enumerate (for/list ([i (in-range 5)])
                       (text "This is a test!")))
          (enumerate #:ordered? #t
                     (for/list ([i (in-range 5)])
                       (text "This is a test!"))))
    (hori x (vert x x)
          (vert x x)
          (vert x x)
          (vert x x)
          (vert x x)))))
