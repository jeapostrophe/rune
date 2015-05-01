#lang racket/base
(require racket/match
         racket/class)

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
    (string-append
     (m   get-shift-down "S")
     (m get-control-down "C")
     (m    get-meta-down "M")
     (m    get-mod3-down "M3")
     (m    get-mod4-down "M4")
     (m    get-mod5-down "M5")
     (m     get-alt-down "A")))
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

(struct cell (fg bg c) #:prefab)
;; xxx close-window
(struct evt:new-window (local-win-id) #:prefab)
;; xxx communicate preferred size (editors no bigger than 80 wide)
(struct evt:new-title (local-win-id title) #:prefab)
(struct evt:key (win ke) #:prefab)
;; xxx clear
(struct evt:resize (win nrows ncols) #:prefab)
(struct evt:write! (win row col c) #:prefab)

(provide (all-defined-out))
