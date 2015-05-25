#lang racket/base
(require racket/match)

(struct event:rune () #:prefab)
(struct event:rune:cmd event:rune (cmd) #:prefab)
(struct event:rune:key event:rune (code) #:prefab)

(define (rk-despace rk)
  (if (eq? '<space> rk)
      #\space
      rk))

(define (key-event->rune-key ke alt? ctrl? meta? shift?)
  (define kc
    (match ke
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
    (if b (format "~a-" c) ""))
  (define mods
    (string-append
     (m shift? "S")
     (m  ctrl? "C")
     (m  meta? "M")
     (m   alt? "A")))
  (define mods?
    (and (not (string=? "" mods))
         (not (and (string=? "S-" mods)
                   (char? kc)))))
  (define kc-e
    (if (symbol? kc)
      (string->symbol (format "<~a>" kc))
      kc))
  (rk-despace
   (and kc-e
        (if mods?
            (string->symbol (format "~a~a" mods kc-e))
            kc-e))))

(provide (all-defined-out))
