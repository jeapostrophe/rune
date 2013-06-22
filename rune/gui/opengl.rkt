#lang racket/base
(require racket/gui/base
         racket/contract
         racket/list
         racket/async-channel
         racket/match
         racket/class
         rune/lib/colors
         rune/lib/context
         rune/lib/timing
         ffi/vector
         opengl
         opengl/tree
         opengl/program
         web-server/templates)

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

(define-opengl-struct bitmapi
  ([bm _uint32]
   [x _float]
   [y _float]
   [w _float]
   [h _float]
   ;; Optimization Idea: It seems like this could be a
   ;; uniform. However, it would be awkward to communicate it from the
   ;; main program to here unless we created an intermediate structure
   ;; that would hold this, but then have it removed before being sent
   ;; to OpenGL. If I did that, then I think I would actually do MORE
   ;; allocation per call, because I'd only save these two floats on
   ;; one side. Overall, I don't think it would be worth it.
   [tw _float]
   [th _float]

   [dx _float]
   [dy _float]

   [vh _sint8]
   [vv _sint8]))
(define (bitmap x y w h bm tw th dx dy)
  (make-bitmapi bm x y w h tw th dx dy 0 0))

(define-opengl-struct outlinei
  ([x _float]
   [y _float]
   [w _float]
   [h _float]

   [c _uint8]

   [vh _sint8]
   [vv _sint8]))
(define (outline x y w h c)
  (make-outlinei x y w h c 0 0))

(define (make-frame colors bg-cr ch)
  (define new-es (make-eventspace))
  (parameterize ([current-eventspace new-es])
    (define rf (new frame% [label ""]))

    (define config
      (new gl-config%))
    (send config set-double-buffered #t)

    (define og-top-draw! void)
    (define c
      (new gf-canvas% [parent rf]
           [gl-config config]
           [style '(gl no-autoclear transparent)]
           [paint-callback
            (λ (c dc)
              (og-top-draw! c dc))]
           [event-ch ch]))
    (send c focus)
    (send rf show #t)

    (define actual-ctxt (send (send c get-dc) get-gl-context))
    (define ctxt (λ (f) (send actual-ctxt call-as-current f)))
    (set-colors-context! colors ctxt)

    (define elements-box (box (void)))
    (match-define (vector bg-r bg-g bg-b) (colors-ref colors bg-cr))

    (define top-draw!
      (ctxt
       (λ ()
         (define-opengl-program BitmapProgram
           #:struct bitmapi
           #:vertex-spec vh vv
           #:uniform BitmapTex 0
           #:dynuniform in_Viewport
           #:attribute in_Position (x y)
           #:attribute in_TexDimension (tw th)
           #:attribute in_Dimension (w h)
           #:attribute in_Vertex (vh vv)
           #:attribute in_Offset (dx dy)
           #:connected TexCoord
           #:vertex (include-template "bvertex.glsl")
           #:fragment (include-template "bfragment.glsl"))
         (define-opengl-program OutlineProgram
           #:struct outlinei
           #:vertex-spec vh vv
           #:uniform ColorTex 1
           #:dynuniform in_Viewport
           #:texture 1 (colors-tex colors)
           #:attribute in_Position (x y)
           #:attribute in_Dimension (w h)
           #:attribute in_Vertex (vh vv)
           #:attribute in_Color (c)
           #:connected Vertex
           #:connected Color
           #:vertex (include-template "overtex.glsl")
           #:fragment (include-template "ofragment.glsl"))

         (λ (c _)
           (define-values (ft ecount)
             (time-it
              (define full-w (send c get-width))
              (define full-h (send c get-height))

              (glPushAttrib (bitwise-ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
              (glViewport 0 0 full-w full-h)

              (glClearColor (exact->inexact (/ bg-r 255))
                            (exact->inexact (/ bg-g 255))
                            (exact->inexact (/ bg-b 255))
                            1.0)

              (glClear (bitwise-ior GL_DEPTH_BUFFER_BIT GL_COLOR_BUFFER_BIT))

              (define outlines empty)
              (define bitmaps (make-hasheq))

              (define ecount
                (tree-iter!
                 (λ (i x)
                   (match x
                     [(? outlinei? o)
                      (set! outlines (cons o outlines))]
                     [(? bitmapi? b)
                      (hash-update! bitmaps (bitmapi-bm b) (λ (x) (cons b x)) empty)]))
                 (unbox elements-box)))

              (with-BitmapProgram
               [#:uniform in_Viewport
                          (f32vector (exact->inexact full-w)
                                     (exact->inexact full-h))]
               (for ([(bm bs) (in-hash bitmaps)])
                 (inner-BitmapProgram
                  [#:texture 0 bm]
                  bs)))

              (OutlineProgram
               [#:uniform in_Viewport
                          (f32vector (exact->inexact full-w)
                                     (exact->inexact full-h))]
               outlines)              

              (glPopAttrib)

              (send actual-ctxt swap-buffers)

              (list ecount (add1 (hash-count bitmaps)))))
           (frame-perf! gf 'frame-draw ft)))))
    (set! og-top-draw! top-draw!)

    (define t (thread (λ () (yield never-evt))))
    (define gf (frame rf c t ctxt elements-box (box "") (make-hasheq)))
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

(define (element? x)
  (or (outlinei? x) (bitmapi? x)))

(provide
 (contract-out
  [outline
   (-> flonum? flonum? flonum? flonum? color/c
       outlinei?)]
  [bitmap
   (-> flonum? flonum? flonum? flonum? exact-nonnegative-integer? flonum? flonum? flonum? flonum?
       bitmapi?)]
  [frame?
   (-> any/c
       boolean?)]
  [rename
   make-frame frame
   ;; Contract Idea: It's conceivable to contract this channel to just
   ;; contain symbols and characters, but I don't think there's a lot
   ;; of value to that.
   (-> colors/c color/c async-channel?
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
   (-> frame? symbol? number?
       void)]
  [frame-refresh!
   (-> frame? string? (tree/c element?)
       void)]))
