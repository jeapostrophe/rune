#lang racket/base
(require racket/contract
         racket/draw
         racket/match
         racket/class
         rune/lib/context
         rune/lib/tree
         rune/lib/colors
         (only-in racket/gui/base make-screen-bitmap))

(struct drawer (ctxt colors font% font-cache-tex font-cache-hash char-width char-height))
(struct glyph (row col fg bg char))
(struct canvas (bg-cr d rrow rcol arow acol bm) #:mutable)

(define (make-drawer ctxt colors face size)
  (define the-font (make-font #:face face #:family 'modern #:size size))
  (define text-bm (make-screen-bitmap 200 200))
  (define text-dc (send text-bm make-dc))
  (send text-dc set-font the-font)
  (define-values (width height xtra-below xtra-above)
    (send text-dc get-text-extent " "))
  (drawer ctxt colors the-font #f (make-hasheq) width height))

(define (make-canvas d bg-cr)
  (canvas bg-cr d -1 -1 -1 -1 #f))

(define (ensure-size! c nrow ncol)
  (define old-rrow (canvas-rrow c))
  (define old-rcol (canvas-rcol c))
  (unless (and (<= nrow old-rrow)
               (<= ncol old-rcol))
    (define d (canvas-d c))
    (define char-width (drawer-char-width d))
    (define char-height (drawer-char-height d))

    (define new-rrow (max (* 2 old-rrow) nrow))
    (define new-rcol (max (* 2 old-rcol) ncol))
    (define new-bm
      ;; OpenGL: Allocate a new texture with this size.
      (make-screen-bitmap
       (inexact->exact (ceiling (* new-rcol char-width)))
       (inexact->exact (ceiling (* new-rrow char-height)))))

    (set-canvas-rrow! c new-rrow)
    (set-canvas-rcol! c new-rcol)
    (set-canvas-bm! c new-bm))

  (set-canvas-arow! c nrow)
  (set-canvas-acol! c ncol))

(define (canvas-bitmap-width c)
  (* (canvas-acol c) (drawer-char-width (canvas-d c))))
(define (canvas-bitmap-height c)
  (* (canvas-arow c) (drawer-char-height (canvas-d c))))

(define (canvas-refresh! c nrow ncol t)
  (ensure-size! c nrow ncol)
  (match-define (canvas bg-cr d _ _ _ _ bm) c)
  (match-define (drawer ctxt colors the-font fc-tex fc-hash char-width char-height) d)

  (define fc-tex-dirty? #f)

  ;; OpenGL: Render to the generated texture
  (define bm-dc (send bm make-dc))

  (define bg-c (colors-ref colors bg-cr))
  (send bm-dc set-background bg-c)
  (send bm-dc clear)
  (send bm-dc set-font the-font)

  ;; OpenGL: Cache a texture atlas of the characters used from the
  ;; font. As you create the vector of things to draw, see if it
  ;; changes. If it doesn't, then you go on, otherwise you need to
  ;; generate it and try again. Maybe recall yourself to make the code
  ;; simpler. Draw with a shader like the Get Bonus shader.
  (define gcount
    (tree-iter!
     (match-lambda
      [(glyph grow gcol fgr bgr char)
       (define x (* gcol char-width))
       (define y (* grow char-height))
       (define w char-width)
       (define h char-height)
       (define fg (colors-ref colors fgr))
       (define bg (colors-ref colors bgr))

       (define ci
         (hash-ref! fc-hash (char->integer char)
                    (λ ()
                      (set! fc-tex-dirty? #t)
                      (hash-count fc-hash))))

       (send bm-dc set-pen bg 0 'solid)
       (send bm-dc set-brush bg 'solid)
       (send bm-dc draw-rectangle x y w h)

       (send bm-dc set-text-foreground fg)
       (send bm-dc draw-text (string char) x y)])
     t))

  (when fc-tex-dirty?
    (eprintf "refreshing font cache texture: ~a\n"
             (hash-count fc-hash)))

  (eprintf "drew ~a glyphs\n" gcount)
  (void))

(define nat? exact-nonnegative-integer?)
(provide
 (contract-out
  (struct glyph
          ([row nat?]
           [col nat?]
           [fg color/c]
           [bg color/c]
           [char char?]))
  [rename
   make-drawer drawer
   (-> context/c colors/c string? nat?
       drawer?)]
  [drawer-char-width
   (-> drawer?
       real?)]
  [drawer-char-height
   (-> drawer?
       real?)]
  [rename
   make-canvas canvas
   (-> drawer?
       color/c
       canvas?)]
  [rename
   canvas-bm canvas-bitmap
   (-> canvas?
       ;; xxx
       (is-a?/c bitmap%))]
  [canvas-bitmap-width
   (-> canvas?
       real?)]
  [canvas-bitmap-height
   (-> canvas?
       real?)]
  [canvas-refresh!
   (-> canvas?
       nat?
       nat?
       (tree/c glyph?)
       void)]))
