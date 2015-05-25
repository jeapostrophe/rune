#lang racket/base
(require racket/match
         racket/async-channel
         rune/app
         rune/ui/web
         rune/events
         rune/buffer
         rune/editor)

(module+ main
  (define to-web-ch (make-async-channel))
  (define m
    (app
     (λ (msg)
       (match msg
         [(event:rune:cmd `(open!))
          (to-web! "document.title = 'Rune';")
          (update-status-on-web!)]
         [_
          (set! status-e (editor-process status-e msg))
          (update-status-on-web!)]))
     to-web-ch))

  (define (to-web! code)
    (async-channel-put to-web-ch code))

  (define status-e (make-editor))
  (define (update-status-on-web!)
    (to-web! (format "document.body.innerHTML = '$ ~a';" (editor->string status-e))))
  
  (start-rune-web m))
