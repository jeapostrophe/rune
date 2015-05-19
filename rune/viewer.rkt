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

(define (start-viewer
         #:opengl-hires? [opengl-hires #f]
         #:scale-factor [scale-factor 1]
         #:font-size [font-size 13.0]
         #:font-face [font-face #f]
         #:color-scheme [color-scheme default-colors]
         #:manager [the-man (start-manager)])
  (define gdb (make-glyph-db))
  (define the-font
    (load-font! gdb
                #:size (* scale-factor font-size)
                #:face font-face
                #:family 'modern))
  (define cgdb (compile-glyph-db gdb))

  (define a-char (font-glyph-idx the-font cgdb #\space))
  (define font-width (glyph-width cgdb a-char))
  (define font-height (glyph-height cgdb a-char))
  (define render (stage-render cgdb))

  (struct viewer (w h sc man)
    #:methods gen:word
    [(define (word-fps w)
       0.0)
     (define (word-label s ft)
       "Rune")
     (define (word-output v)
       (match-define (viewer w h sc man) v)
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
         (printf "actual size ~v\n" (vector w h))
         (printf "scaled size ~v\n" (vector (* scale-factor w) (* scale-factor h)))
         (r (* scale-factor w) (* scale-factor h) dc)))
     (define (word-evt v)
       (match-define (viewer w h sc man) v)
       (manager-evt man))
     (define (word-event v e)
       (match-define (viewer w h sc man) v)
       (match e
         [(or 'close
              (and (? key-event?)
                   (app key-event-code 'escape)))
          #f]
         [(? key-event?)
          (define rk (key-event->rune-key e))
          (when rk
            (manager-key-event! man rk))
          v]
         [(evt:write! _ cmd)
          (screen-write! sc cmd)
          v]
         [`(resize ,snw ,snh)
          (printf "resize ~v\n" (cons snw snh))
          (define nw (* scale-factor snw))
          (define nh (* scale-factor snh))
          (printf "scaled resize ~v\n" (cons nw nh))
          (cond
            [(and (= w nw) (= h nh))
             v]
            [else
             (define nrows (- (inexact->exact (floor (/ nh font-height))) 1))
             (define ncols (inexact->exact (floor (/ nw font-width))))
             (define nsc (make-screen #:rows nrows #:cols ncols))
             (screen-copy! sc nsc)
             (manager-resize! man nrows ncols)
             (struct-copy viewer v
                          [w nw] [h nh]
                          [sc nsc])])]
         [_
          v]))])

  (call-with-chaos
   (make-gui #:mode gui-mode
             #:opengl-hires? opengl-hires)
   (λ ()
     (fiat-lux
      (viewer 0 0 (make-screen #:rows 0 #:cols 0)
              the-man)))))

(provide start-viewer)
