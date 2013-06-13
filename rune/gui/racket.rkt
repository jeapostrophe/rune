#lang racket/base
(require racket/gui/base
         racket/contract
         racket/async-channel
         racket/match
         racket/class
         rune/lib/tree
         rune/lib/timing)

(struct frame (frame% canvas% thread elements-box label-box perf-hash))

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
(struct bitmap element (bm dx dy))

(define (make-frame ch)
  (define new-es (make-eventspace))
  (parameterize ([current-eventspace new-es])
    (define rf (new frame% [label ""]))
    (define elements-box (box (void)))
    (define (top-draw! c dc)
      (send dc clear)
      (define-values (ft ecount)
        (time-it
         (tree-iter!
          (match-lambda
           [(outline x y w h c)
            (send dc set-pen c 2 'solid)
            (send dc set-brush c 'transparent)
            (send dc draw-rectangle x y w h)]
           [(bitmap x y w h bm dx dy)
            (send dc draw-bitmap-section bm x y dx dy w h)])
          (unbox elements-box))))
      (when gf (frame-perf! gf 'frame-draw ft))
      (eprintf "drew ~a elements\n" ecount))
    (define c
      (new gf-canvas% [parent rf]
           [style '(no-autoclear transparent)]
           [paint-callback top-draw!]
           [event-ch ch]))
    (send c focus)
    (send rf show #t)
    (define t (thread (λ () (yield never-evt))))
    (define gf (frame rf c t elements-box (box "") (make-hasheq)))
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
           ;; xxx
           [c (is-a?/c color%)]))
  (struct bitmap
          ([x real?]
           [y real?]
           [w real?]
           [h real?]
           ;; xxx
           [bm (is-a?/c bitmap%)]
           [dx real?]
           [dy real?]))
  [frame?
   (-> any/c
       boolean?)]
  [rename
   make-frame frame
   (-> async-channel? ;; xxx contract to correct symbols
       frame?)]
  [frame-width
   (-> frame?
       exact-nonnegative-integer?)]
  [frame-height
   (-> frame?
       exact-nonnegative-integer?)]
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
