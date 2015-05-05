#lang racket/base
(require racket/match
         rune/viewer
         rune/manager
         rune/colors)

(module+ main
  (define main-km
    (λ (ke)
      (match ke
        [ke
         (printf "km: ~a\n" ke)])))
  (define m
    (start-manager
     #:keymap main-km))
  #;#;
  (define status-bar
    (start-status-bar))
  (define command-line
    (start-command-line))
  (define v
    (start-viewer
     #:font-face "Triplicate T4c"
     #:opengl-hires? #t
     #:scale-factor 2
     #:color-scheme solarized-light
     #:manager m)))
