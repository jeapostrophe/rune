#lang racket/base
(require pict
         icns)

(define logo
  (text "‮ᚱ‬‬"))

(display
 (pict->icns-bytes
  logo))
