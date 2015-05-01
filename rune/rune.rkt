#lang racket/base
(require rune/viewer
         rune/manager
         rune/colors)

(module+ main
  (start-viewer
   #:font-face "Triplicate T4c"
   #:color-scheme solarized-light
   #:manager (start-manager)))
