#lang racket/base
(require racket/file
         racket/string
         racket/flonum
         racket/fixnum
         racket/match
         gfx/color
         mode-tau
         rune/colors
         rune/manager
         rune/events
         lux
         lux/chaos/gui
         lux/chaos/gui/key)

(struct screen (rows cols row-vector))
(define default-cell #f)

(define (make-screen #:rows rows #:cols cols)
  (screen rows cols
          (build-vector rows
                        (λ (rowi)
                          (make-row #:cols cols)))))
(define (make-row #:cols cols)
  (make-vector cols default-cell))
(define (screen-write! sc row col c)
  (match-define (screen rows cols row-vector) sc)
  (when (and (< row rows) (< col cols))
    (vector-set! (vector-ref row-vector row) col c)))
(define (screen-copy! source dest)
  (for ([row (in-vector (screen-row-vector source))]
        [rowi (in-naturals)])
    (for ([c (in-vector row)]
          [coli (in-naturals)])
      (screen-write! dest rowi coli c))))

(define (start-viewer
         #:font-size [font-size 13.0]
         #:font-face [font-face #f]
         #:color-scheme [color-scheme default-colors]
         #:manager [the-man (start-manager)])
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

  (struct viewer (w h sc man)
    #:methods gen:word
    [(define (word-fps w)
       0.0)
     (define (word-label s ft)
       (lux-standard-label "Rune" ft))
     (define (word-output v)
       (match-define (viewer w h sc man) v)
       (define bg (colors-ref color-scheme BG))
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
          (struct-copy viewer v
                       [man (manager-key-event man (key-event->rune-key e))])]
         [(comm:screen-write! row col c)
          (screen-write! sc row col c)
          v]
         [`(resize ,nw ,nh)
          (cond
            [(and (= w nw) (= h nh))
             v]
            [else
             (define nrows (- (inexact->exact (floor (/ nh font-height))) 1))
             (define ncols (inexact->exact (floor (/ nw font-width))))
             (define nsc (make-screen #:rows nrows #:cols ncols))
             (screen-copy! sc nsc)
             (struct-copy viewer v
                          [man (manager-resize man nrows ncols)]
                          [w nw] [h nh]
                          [sc nsc])])]
         [_
          v]))])

  (call-with-chaos
   (make-gui #:mode gui-mode)
   (λ ()
     (fiat-lux
      (viewer 0 0 (make-screen #:rows 0 #:cols 0)
              the-man)))))

(provide start-viewer)
