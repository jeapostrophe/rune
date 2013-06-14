#lang racket/base
(require racket/contract
         racket/draw
         racket/match
         racket/class
         rune/lib/tree
         (only-in racket/gui/base make-screen-bitmap))

(struct drawer (font% char-width char-height))
(struct glyph (row col fg bg char))
(struct canvas (bg-c d rrow rcol arow acol bm bm-dc) #:mutable)

(define (make-drawer face size)
  (define the-font (make-font #:face face #:family 'modern #:size size))
  (define text-bm (make-screen-bitmap 200 200))
  (define text-dc (send text-bm make-dc))
  (send text-dc set-font the-font)
  (define-values (width height xtra-below xtra-above)
    (send text-dc get-text-extent " "))
  (drawer the-font width height))

(define (make-canvas d bg-c)
  (match-define (drawer the-font char-width char-height) d)
  (ensure-size! (canvas bg-c d 0 0 0 0 #f #f) 0 0))

(define (ensure-size! c nrow ncol)
  (match-define (canvas bg-c (drawer the-font char-width char-height)
                        old-rrow old-rcol old-arow old-acol old-bm old-bm-dc)
                c)
  (unless (and (<= nrow old-rrow)
               (<= ncol old-rcol))
    (define new-rrow (max (* 2 old-rrow) nrow))
    (define new-rcol (max (* 2 old-rcol) ncol))
    (define new-bm
      (make-screen-bitmap
       (inexact->exact (ceiling (* new-rcol char-width)))
       (inexact->exact (ceiling (* new-rrow char-height)))))
    (define new-bm-dc (send new-bm make-dc))
    (send new-bm-dc set-background bg-c)
    (send new-bm-dc clear)
    (send new-bm-dc set-font the-font)

    (when old-bm
      (send new-bm-dc draw-bitmap-section old-bm
            0 0
            0 0
            (send old-bm get-width)
            (send old-bm get-height)))

    (set-canvas-rrow! c new-rrow)
    (set-canvas-rcol! c new-rcol)
    (set-canvas-bm! c new-bm)
    (set-canvas-bm-dc! c new-bm-dc))

  (set-canvas-arow! c nrow)
  (set-canvas-acol! c ncol)
  c)

(define (canvas-bitmap-width c)
  (match-define (canvas _ (drawer _ char-width char-height) _ _ arow acol _ bm-dc) c)
  (* acol char-width))
(define (canvas-bitmap-height c)
  (match-define (canvas _ (drawer _ char-width char-height) _ _ arow acol _ bm-dc) c)
  (* arow char-width))

(define (canvas-refresh! c nrow ncol t)
  (match-define (canvas _ (drawer _ char-width char-height) _ _ _ _ _ bm-dc)
                (ensure-size! c nrow ncol))
  ;; xxx remove if i do "smart" redisplay
  (send bm-dc clear)
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
   (-> string? nat?
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
       (is-a?/c color%)
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
