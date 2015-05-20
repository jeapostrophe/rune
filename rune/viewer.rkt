#lang racket/base
(require racket/file
         racket/string
         racket/flonum
         racket/fixnum
         racket/match
         gfx/color
         mode-tau
         rune/screen
         rune/colors
         rune/manager
         rune/events
         lux
         lux/chaos/gui
         lux/chaos/gui/key)

(struct vds (cgdb the-font font-width font-height render))

(define (make-viewer-drawing-state
         #:size font-size
         #:face font-face)
  (define gdb (make-glyph-db))
  (define the-font
    (load-font! gdb
                #:size font-size
                #:face font-face
                #:family 'modern))
  (define cgdb (compile-glyph-db gdb))

  (define a-char (font-glyph-idx the-font cgdb #\space))
  (define font-width (glyph-width cgdb a-char))
  (define font-height (glyph-height cgdb a-char))
  (define render (stage-render cgdb))

  (vds cgdb the-font font-width font-height render))

(define (start-viewer
         #:opengl-hires? [opengl-hires #f]
         #:font-sizes [font-sizes '(13.0)]
         #:font-face [font-face #f]
         #:color-scheme [color-scheme default-colors]
         #:manager [the-man (start-manager)])

  (struct viewer (font-idx vds w h sc man)
    #:methods gen:word
    [(define (word-fps w)
       0.0)
     (define (word-label s ft)
       "Rune")
     (define (word-output v)
       (match-define (viewer _ the-vds w h sc man) v)
       (match-define (vds cgdb the-font font-width font-height render) the-vds)
       (define bg (colors-ref color-scheme BG))
       (define r
         (render
          (red bg) (green bg) (blue bg)
          ;; NOTE This could be optimized a lot more because it will be
          ;; the same size a lot of the time.
          (for/list ([row (in-vector (screen-row-vector sc))]
                     [rowi (in-naturals)])
            (for/list ([c (in-vector row)]
                       [coli (in-naturals)])
              (match c
                [#f #f]
                [(cell fgc bgc char)
                 (define fg (colors-ref color-scheme fgc))
                 (define bg (colors-ref color-scheme bgc))
                 (glyph (fl* (fl+ 0.5 (fx->fl coli)) font-width)
                        (fl* (fl+ 0.5 (fx->fl rowi)) font-height)
                        (font-glyph-idx the-font cgdb char)
                        #:fgr (red fg) #:fgg (green fg) #:fgb (blue fg)
                        #:bgr (red bg) #:bgg (green bg) #:bgb (blue bg))])))))
       (λ (w h dc)
         (r w h dc)))
     (define (word-evt v)
       (match-define (viewer _ _ _ _ _ man) v)
       (manager-evt man))
     (define (word-event v e)
       (match-define (viewer _ the-vds w h sc man) v)
       (match e
         [(or 'close
              (and (? key-event?)
                   (app key-event-code 'escape)))
          #f]
         [(? key-event?)
          (define rk (key-event->rune-key e))
          (match rk
            ;; xxx customize keys
            ['M-=
             (change-font v +1)]
            ['M--
             (change-font v -1)]
            [_
             (when rk
               (manager-key-event! man rk))
             v])]
         [(evt:write! _ cmd)
          (screen-write! sc cmd)
          v]
         [`(resize ,nw ,nh)
          (cond
            [(and (= w nw) (= h nh))
             v]
            [else
             (resize-viewer
              (struct-copy viewer v
                           [w nw] [h nh]))])]
         [_
          v]))])

  (define (change-font v size-delta)
    (initialize-vds
     (struct-copy viewer v
                  [font-idx
                   (modulo (+ (viewer-font-idx v) size-delta)
                           (length font-sizes))])))

  (define (initialize-vds v)
    (define font-size (list-ref font-sizes (viewer-font-idx v)))
    (resize-viewer
     (struct-copy viewer v
                  [vds (make-viewer-drawing-state
                        #:size font-size
                        #:face font-face)])))

  (define (resize-viewer v)
    (match-define (viewer _ the-vds w h sc man) v)
    (match-define (vds cgdb the-font font-width font-height render) the-vds)
    (define nrows (inexact->exact (floor (/ h font-height))))
    (define ncols (inexact->exact (floor (/ w font-width))))
    (define nsc (make-screen #:rows nrows #:cols ncols))
    (manager-resize! man nrows ncols)
    (struct-copy viewer v [sc nsc]))

  (call-with-chaos
   (make-gui #:mode gui-mode
             #:opengl-hires? opengl-hires)
   (λ ()
     (fiat-lux
      (initialize-vds
       (viewer 0 #f
               0 0
               (make-screen #:rows 0 #:cols 0)
               the-man))))))

(provide start-viewer)
