#lang racket/base
(require rune/manager
         rune/ui/web)

(module+ main
  (define m
    (manager
     (λ (msg)
       (displayln msg))
     never-evt))
  
  (start-rune-web m))
