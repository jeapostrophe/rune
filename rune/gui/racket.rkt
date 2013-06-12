#lang racket/base
(require racket/gui/base
         racket/contract
         racket/async-channel
         racket/match
         racket/class)

(struct frame (f c t pb))

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
    (define gf (new frame% [label ""]))
    (define paint-box (box (void)))
    (define (top-draw! c dc)
      (send dc clear)
      (iter-draw! dc (unbox paint-box)))
    (define (iter-draw! dc es)
      (match es
        [(or #f (? void?) (list))
         (void)]
        [(cons a d)
         (iter-draw! dc a)
         (iter-draw! dc d)]
        [(outline x y w h c)
         (send dc set-pen c 2 'solid)
         (send dc set-brush c 'transparent)
         (send dc draw-rectangle x y w h)]
        [(bitmap x y w h bm dx dy)
         (send dc draw-bitmap-section bm x y dx dy w h)]))
    (define c (new gf-canvas% [parent gf]
                   [style '(no-autoclear transparent)]
                   [paint-callback top-draw!]
                   [event-ch ch]))
    (send c focus)
    (send gf show #t)
    (define t (thread (λ () (yield never-evt))))
    (frame gf c t paint-box)))

(define (frame-width gf)
  (send (frame-c gf) get-width))
(define (frame-height gf)
  (send (frame-c gf) get-height))

(define (frame-refresh! gf l es)
  (send (frame-f gf) set-label l)
  (set-box! (frame-pb gf) es)
  (send (frame-c gf) refresh-now))

;; xxx
(define (tree/c ?)
  any/c)

(provide
 (contract-out
  (struct outline
          ([x real?]
           [y real?]
           [w real?]
           [h real?]
           [c (is-a?/c color%)]))
  (struct bitmap
          ([x real?]
           [y real?]
           [w real?]
           [h real?]
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
  [frame-refresh!
   (-> frame?
       string?
       (tree/c element?)
       void)]))
