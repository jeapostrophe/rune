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
         (printf "km: ~v\n" ke)])))
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
     #:font-sizes '(13.0 26.0)
     #:font-face "Triplicate T4c"
     #:opengl-hires? #t
     #:color-scheme solarized-light
     #:manager m)))
