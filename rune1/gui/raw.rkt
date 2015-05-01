#lang racket/base
(require racket/gui/base
         racket/contract
         racket/list
         racket/async-channel
         racket/match
         racket/class
         opengl/tree
         rune/lib/colors
         rune/lib/context
         rune/lib/timing)

(struct frame (frame% canvas% thread context elements-box label-box perf-hash))

(define (key-event->rune-key ke)
  (define kc
    (match (send ke get-key-code)
      [#\nul #f]
      [#\rubout 'delete]
      [#\backspace 'backspace]
      [#\space 'space]
      [#\return 'return]
      [#\tab 'tab]
      [(? char? c) c]
      ['release #f]
      [(? symbol? s) s]))
  (define-syntax-rule (m b c)
    (if (send ke b) (format "~a-" c) ""))
  (define mods
    (format "~a~a~a"
            (m get-control-down "C")
            (m    get-meta-down "M")
            (m   get-shift-down "S")))
  (define mods?
    (and (not (string=? "" mods))
         (not (and (string=? "S-" mods)
                   (char? kc)))))
  (define kc-e
    (if (symbol? kc)
      (string->symbol (format "<~a>" kc))
      kc))
  (and kc-e
       (if mods?
         (string->symbol (format "~a~a" mods kc-e))
         kc-e)))

(define gf-canvas%
  (class canvas%
    (init-field event-ch)

    (define/override (on-char event)
      (define ek (key-event->rune-key event))
      (when ek
        (async-channel-put event-ch ek)))
    (define/override (on-size w h)
      (async-channel-put event-ch '<resize>))

    (super-new)))

(struct element (x y w h))
(struct outline element (c))
(struct bitmap element (bm tw th dx dy))

(define (make-frame colors bg-cr ch)
  (define new-es (make-eventspace))
  ;; OpenGL: Configure an OpenGL canvas
  (parameterize ([current-eventspace new-es])
    (define rf (new frame% [label ""]))
    (define elements-box (box (void)))
    (define ctxt (λ (f) (f)))
    (define (top-draw! c dc)
      ;; OpenGL: You need the dc to get the context, so cache whether
      ;; you've done this yet.
      (set-colors-context! colors ctxt)

      ;; OpenGL: Render to the screen
      (define bg-c (colors-ref% colors bg-cr))
      (send dc set-background bg-c)
      (send dc clear)
      (define-values (ft ecount)
        (time-it
         (let ()
           (define outlines empty)
           (define bitmaps (make-hasheq))

           (define ecount
             (tree-iter!
              (match-lambda
               [(? outline? o)
                (set! outlines (cons o outlines))]
               [(? bitmap? b)
                (hash-update! bitmaps (bitmap-bm b) (λ (x) (cons b x)) empty)])
              (unbox elements-box)))

           ;; OpenGL: Turn on the bitmap shader
           (for ([(bm bs) (in-hash bitmaps)])
             ;; OpenGL: Bind the texture for 'bm' and then send this
             ;; worklist to the shader
             (for ([b (in-list bs)])
               (match-define (bitmap x y w h bm tw th dx dy) b)
               (send dc draw-bitmap-section bm x y dx dy (max 0 w) (max 0 h))))

           ;; OpenGL: Turn on the outine shader and send this worklist
           (for ([o (in-list outlines)])
             (match-define (outline x y w h cr) o)
             (define c (colors-ref% colors cr))
             (send dc set-pen c 2 'solid)
             (send dc set-brush c 'transparent)
             (send dc draw-rectangle x y w h))

           (list ecount (add1 (hash-count bitmaps))))))
      (frame-perf! gf 'frame-draw ft)
      (eprintf "drew ~a (elements, distinct things)\n" ecount))
    (define c
      (new gf-canvas% [parent rf]
           [style '(no-autoclear transparent)]
           [paint-callback top-draw!]
           [event-ch ch]))
    (define t (thread (λ () (yield never-evt))))
    (define gf (frame rf c t ctxt elements-box (box "") (make-hasheq)))
    (send c focus)
    (send rf show #t)
    gf))

(define (frame-width gf)
  (send (frame-canvas% gf) get-width))
(define (frame-height gf)
  (send (frame-canvas% gf) get-height))

(define (frame-refresh! gf l es)
  (set-box! (frame-label-box gf) l)
  (update-label! gf)
  (set-box! (frame-elements-box gf) es)
  (define-values (rt _)
    (time-it (send (frame-canvas% gf) refresh-now)))
  (frame-perf! gf 'frame-render rt))

(define (frame-perf! gf k v)
  (hash-set! (frame-perf-hash gf) k v)
  (update-label! gf))

(define (update-label! gf)
  (define h (frame-perf-hash gf))
  (define (tl k)
    (real->decimal-string (hash-ref h k 0)))
  (define l
    (format "K~a/T~a/B~a/E~a/D~a/F~a: ~a"
            (tl 'key-handling)
            (tl 'transform)
            (tl 'buffers)
            (tl 'elements)
            (tl 'frame-draw)
            (real->decimal-string
             (- (hash-ref h 'frame-render 0)
                (hash-ref h 'frame-draw 0)))
            (unbox (frame-label-box gf))))
  (send (frame-frame% gf) set-label l))

(provide
 (contract-out
  (struct outline
          ([x real?]
           [y real?]
           [w real?]
           [h real?]
           [c color/c]))
  (struct bitmap
          ([x real?]
           [y real?]
           [w real?]
           [h real?]
           [bm (is-a?/c bitmap%)]
           [tw real?]
           [th real?]
           [dx real?]
           [dy real?]))
  [frame?
   (-> any/c
       boolean?)]
  [rename
   make-frame frame
   (-> colors/c color/c
       async-channel?
       frame?)]
  [frame-width
   (-> frame?
       exact-nonnegative-integer?)]
  [frame-height
   (-> frame?
       exact-nonnegative-integer?)]
  [frame-context
   (-> frame?
       context/c)]
  [frame-perf!
   (-> frame?
       symbol?
       number?
       void)]
  [frame-refresh!
   (-> frame?
       string?
       (tree/c element?)
       void)]))
