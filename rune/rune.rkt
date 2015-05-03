#lang racket/base
(require rune/viewer
         rune/manager
         rune/colors)

(module+ main
  (define m
    (start-manager))
  (define v
    (start-viewer
     #:font-face "Triplicate T4c"
     #:opengl-hires? #t
     #:scale-factor 2
     #:color-scheme solarized-light
     #:manager m)))
