#lang racket/base
(require racket/contract
         racket/draw
         racket/match
         racket/class
         rune/lib/tree
         (only-in racket/gui/base make-screen-bitmap))

(struct drawer (font% char-width char-height))
(struct glyph (row col fg bg char))
(struct canvas (char-width char-height arow acol bm bm-dc) #:mutable)

(define (make-drawer face)
  ;; xxx face
  (define the-font (make-font #:family 'modern))
  (define text-bm (make-screen-bitmap 200 200))
  (define text-dc (send text-bm make-dc))
  (send text-dc set-font the-font)
  (define-values (width height xtra-below xtra-above)
    (send text-dc get-text-extent " "))
  (drawer the-font width height))

(define (make-canvas d erow ecol)
  (match-define (drawer the-font char-width char-height) d)
  (define arow (* 2 erow))
  (define acol (* 2 ecol))
  (define bm
    (make-screen-bitmap
     (inexact->exact (ceiling (* acol char-width)))
     (inexact->exact (ceiling (* arow char-height)))))
  (define bm-dc (send bm make-dc))
  (send bm-dc clear)
  (send bm-dc set-font the-font)
  (canvas char-width char-height arow acol bm bm-dc))

(define (canvas-refresh! c nrow ncol t)
  ;; xxx check nrow & ncol
  (match-define (canvas char-width char-height arow acol bm bm-dc) c)
  (define gcount
    (tree-iter!
     (match-lambda
      [(glyph grow gcol fg bg char)
       (define x (* gcol char-width))
       (define y (* grow char-height))
       (define w char-width)
       (define h char-height)

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
           [fg (is-a?/c color%)]
           [bg (is-a?/c color%)]
           [char char?]))
  [rename
   make-drawer drawer
   (-> string?
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
       nat?
       nat?
       canvas?)]
  [rename
   canvas-bm canvas-bitmap
   (-> canvas?
       ;; xxx
       (is-a?/c bitmap%))]
  [canvas-refresh!
   (-> canvas?
       nat?
       nat?
       (tree/c glyph?)
       void)]))
