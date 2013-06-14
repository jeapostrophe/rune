#lang racket/base
(require racket/contract
         racket/draw
         racket/match
         racket/class
         rune/lib/context
         rune/lib/tree
         rune/lib/colors
         (only-in racket/gui/base make-screen-bitmap))

(struct drawer (ctxt colors font% char-width char-height))
(struct glyph (row col fg bg char))
(struct canvas (bg-cr d rrow rcol arow acol bm) #:mutable)

(define (make-drawer ctxt colors face size)
  (define the-font (make-font #:face face #:family 'modern #:size size))
  (define text-bm (make-screen-bitmap 200 200))
  (define text-dc (send text-bm make-dc))
  (send text-dc set-font the-font)
  (define-values (width height xtra-below xtra-above)
    (send text-dc get-text-extent " "))
  (drawer ctxt colors the-font width height))

(define (make-canvas d bg-cr)
  (match-define (drawer _ _ _ char-width char-height) d)
  (canvas bg-cr d -1 -1 -1 -1 #f))

(define (ensure-size! c nrow ncol)
  (match-define (canvas _ (drawer _ _ _ char-width char-height)
                        old-rrow old-rcol old-arow old-acol _)
                c)
  (unless (and (<= nrow old-rrow)
               (<= ncol old-rcol))
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
  (match-define (canvas _ (drawer _ _ _ char-width char-height) _ _ arow acol _) c)
  (* acol char-width))
(define (canvas-bitmap-height c)
  (match-define (canvas _ (drawer _ _ _ char-width char-height) _ _ arow acol _) c)
  (* arow char-height))

(define (canvas-refresh! c nrow ncol t)
  (ensure-size! c nrow ncol)
  (match-define (canvas bg-cr (drawer _ colors the-font char-width char-height)
                        _ _ _ _ bm)
                c)

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

       (send bm-dc set-pen bg 0 'solid)
       (send bm-dc set-brush bg 'solid)
       (send bm-dc draw-rectangle x y w h)

       (send bm-dc set-text-foreground fg)
       (send bm-dc draw-text (string char) x y)])
     t))
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
