#lang racket/base
(require racket/gui/base
         racket/async-channel
         racket/match
         racket/class)

(struct gframe (f c pb))

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

    (super-new)))

(define (gui-frame ch)
  (define gf (new frame% [label ""]))
  (define paint-box (box void))
  (define c (new gf-canvas% [parent gf]
                 [style '(no-autoclear transparent)]
                 [paint-callback
                  (λ (c dc) ((unbox paint-box)
                             (send c get-width)
                             (send c get-height)
                             dc))]
                 [event-ch ch]))
  (send c focus)
  (send gf show #t)
  (gframe gf c paint-box))

(define (set-gui-frame-label! gf i [prepend? #f])
  (define l
    (if prepend?
      (format "~a: ~a" i (send (gframe-f gf) get-label))
      i))
  (send (gframe-f gf) set-label l))

(define (gui-frame-refresh! gf f)
  (set-box! (gframe-pb gf) f)
  (send (gframe-c gf) refresh-now))

(define gui-sync yield)

(provide gui-frame
         gui-sync
         set-gui-frame-label!
         gui-frame-refresh!)
