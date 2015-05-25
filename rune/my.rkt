#lang racket/base
(require rune/manager
         rune/ui/web
         rune/buffer
         rune/editor)

(module+ main
  (define m
    (manager
     (λ (msg)
       
       (writeln msg))
     never-evt))
  
  (start-rune-web m))
