#lang racket/base
(require racket/contract
         racket/draw
         racket/match
         racket/list
         racket/class
         rune/lib/context
         rune/lib/colors
         (only-in racket/gui/base make-screen-bitmap)
         opengl/texture-render
         opengl/texture
         opengl/program
         opengl/tree
         opengl
         web-server/templates)
(module+ test
  (require rackunit))

(struct drawer
        (ctxt colors
              font% font-cache-tex font-cache-hash [font-cache-side #:mutable]
              char-width char-height))
(struct canvas
        (bg-cr d rrow rcol arow acol bm gp)
        #:mutable)

(define (make-drawer ctxt colors face size)
  (define the-font (make-font #:face face #:family 'modern #:size size))
  (define text-bm (make-screen-bitmap 200 200))
  (define text-dc (send text-bm make-dc))
  (send text-dc set-font the-font)
  (define-values (width height xtra-below xtra-above)
    (send text-dc get-text-extent " "))
  (define fc-tex
    (ctxt
     (λ ()
       (allocate-texture))))
  (drawer ctxt colors the-font fc-tex (make-hasheq) 0 width height))

(define (make-canvas d bg-cr)
  (define-values (canvas-tex gp)
    ((drawer-ctxt d)
     (λ ()
       (define-opengl-program GlyphProgram
         #:struct glyphi
         #:vertex-spec vh vv
         #:uniform CharWidth (drawer-char-width d)
         #:uniform CharHeight (drawer-char-height d)
         #:uniform ColorTex 1
         #:uniform FontTex 2
         #:texture 1 (colors-tex (drawer-colors d))
         #:texture 2 (drawer-font-cache-tex d)
         #:attribute in_Position_rc (row col)
         #:attribute in_Char (char)
         #:attribute in_Vertex (vh vv)
         #:attribute in_Color (f*b)
         #:attribute in_Viewport_rc (mrow mcol)
         #:connected FColor
         #:connected BColor
         #:connected TexCoord
         #:vertex (include-template "gvertex.glsl")
         #:fragment (include-template "gfragment.glsl"))

       (values (allocate-render-texture 0 0)
               (λ (gs)
                 (with-GlyphProgram
                  ;; xxx see if i can make this based on whether the fc is dirty?

                  ;; xxx integrate to inner
                  (glUniform1f (glGetUniformLocation GlyphProgramId "CharSide")
                               (drawer-font-cache-side d))
                  (inner-GlyphProgram gs)))))))
  (canvas bg-cr d -1 -1 -1 -1 canvas-tex gp))

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
    ((drawer-ctxt d)
     (λ ()
       (resize-render-texture!
        (canvas-bm c)
        (inexact->exact (ceiling (* new-rcol char-width)))
        (inexact->exact (ceiling (* new-rrow char-height))))))

    (set-canvas-rrow! c new-rrow)
    (set-canvas-rcol! c new-rcol))

  (set-canvas-arow! c nrow)
  (set-canvas-acol! c ncol))

(define (canvas-bitmap c)
  (render-texture-t (canvas-bm c)))
(define (canvas-bitmap-width c)
  (* (canvas-acol c) (drawer-char-width (canvas-d c))))
(define (canvas-bitmap-height c)
  (* (canvas-arow c) (drawer-char-height (canvas-d c))))
(define (canvas-real-width c)
  (* (canvas-rcol c) (drawer-char-width (canvas-d c))))
(define (canvas-real-height c)
  (* (canvas-rrow c) (drawer-char-height (canvas-d c))))

(define-opengl-struct glyphi
  ;; xxx remove floats
  ([row _float]
   [col _float]
   ;; xxx move to uniform
   [mrow _float]
   [mcol _float]

   [f*b _uint16]
   ;; xxx could be smaller, but big for alignment
   [char _uint16]

   [vh _sint8]
   [vv _sint8]))

(define (nibbles hi lo)
  (+ (arithmetic-shift hi 4) lo))
(define (nibble-lo n)
  (bitwise-and n #x0F))
(define (nibble-hi n)
  (bitwise-and (arithmetic-shift n -4) #x0F))

(module+ test
  (for* ([x (in-range 16)]
         [y (in-range 16)])
    (define n (nibbles x y))
    (check-pred byte? n)
    (check-equal? (nibble-hi n) x)
    (check-equal? (nibble-lo n) y)))

(require racket/runtime-path)
(define-runtime-path font.png "font.png")
(define (make-font-bitmap the-font% char-width char-height fc-hash)
  (define k (hash-count fc-hash))
  (define side-raw (sqrt k))
  (define side (ceiling side-raw))
  (define bm
    (make-screen-bitmap (inexact->exact (* side char-width))
                        (inexact->exact (* side char-height))))
  (define bm-dc (send bm make-dc))
  (send bm-dc set-font the-font%)

  (for ([(cn ci) (in-hash fc-hash)])
    (define cx (modulo ci side))
    (define cy (quotient ci side))
    (send bm-dc draw-text
          (string (integer->char cn))
          (* cx char-width)
          (* cy char-height)))
  
  (send bm save-file font.png 'png 100)

  (values side bm))

(struct glyph (row col fg bg char))

(define (canvas-refresh! c nrow ncol t)
  (ensure-size! c nrow ncol)
  (match-define (canvas bg-cr d rrow rcol (== nrow) (== ncol) bm GlyphProgram) c)
  (match-define (drawer ctxt colors the-font fc-tex fc-hash old-fc-side
                        char-width char-height) d)

  (define fc-tex-dirty? #f)
  (define gs empty)

  (define gcount
    (tree-iter!
     (lambda (i g)
       (match-define (glyph grow gcol fgr bgr char) g)
       (define ci
         (hash-ref! fc-hash (char->integer char)
                    (λ ()
                      (set! fc-tex-dirty? #t)
                      (hash-count fc-hash))))
       (set! gs
             (cons (make-glyphi (exact->inexact grow)
                                (exact->inexact gcol)
                                (exact->inexact rrow)
                                (exact->inexact rcol)
                                (nibbles fgr bgr) ci 0 0)
                   gs)))
     t))

  ((drawer-ctxt d)
   (λ ()
     (when fc-tex-dirty?
       (eprintf "refreshing font cache texture: ~a\n"
                (hash-count fc-hash))
       (define-values (new-fc-side fc-bm)
         (make-font-bitmap the-font char-width char-height fc-hash))
       (set-drawer-font-cache-side! d new-fc-side)
       (load-texture/bitmap fc-bm #:texture fc-tex))

     (match-define (vector bg-r bg-g bg-b) (colors-ref colors bg-cr))
     (with-texture-to-render
      bm
      (glPushAttrib (bitwise-ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
      (glClearColor (exact->inexact (/ bg-r 255))
                    (exact->inexact (/ bg-g 255))
                    (exact->inexact (/ bg-b 255))
                    1.0)
      (glClear (bitwise-ior GL_DEPTH_BUFFER_BIT GL_COLOR_BUFFER_BIT))

      (GlyphProgram gs)

      (glPopAttrib))))

  (eprintf "drew ~a glyphs\n" gcount)
  (void))

(define nat? exact-nonnegative-integer?)
(provide
 (contract-out
  [glyph
   (-> nat? nat? color/c color/c char?
       glyph?)]
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
   (-> drawer? color/c
       canvas?)]
  [canvas-bitmap
   (-> canvas?
       exact-nonnegative-integer?)]
  [canvas-bitmap-width
   (-> canvas?
       real?)]
  [canvas-bitmap-height
   (-> canvas?
       real?)]
  [canvas-real-width
   (-> canvas?
       real?)]
  [canvas-real-height
   (-> canvas?
       real?)]
  [canvas-refresh!
   (-> canvas? nat? nat? (tree/c glyph?)
       void)]))
