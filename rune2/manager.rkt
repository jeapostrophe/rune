#lang racket/base
(require racket/runtime-path
         racket/match
         rune2/common)

(define RACKET-PATH (find-executable-path "racket"))
(define-runtime-path gui-path "gui.rkt")

(module+ main
  (define-values (sp stdout stdin _3)
    (subprocess #f #f (current-error-port)
                RACKET-PATH
                "-t" gui-path))

  (define (send! c)
    (write c stdin)
    (newline stdin)
    (flush-output stdin))
  (define (uzbl-send! name cmd)
    (send! (command:uzbl:send name cmd)))

  (define-runtime-path here ".")
  (define (here-uri h)
    (format "uri ~a"
            (path->string
             (build-path here h))))

  (uzbl-send! 'top (here-uri "top.html"))
  (send! (command:uzbl:attach 'body))
  (uzbl-send! 'body "uri http://google.com")
  (uzbl-send! 'bot (here-uri "bot.html"))

  (thread
   (λ ()
     (for ([i (in-range 100)])
       (uzbl-send! 'bot (format "set inject_html = <strong>~a</strong>" i))
       (sleep 1))))

  (define process
    (match-lambda
     ;; I'm 70% sure I don't want these
     [(event:uzbl (or 'bot 'top)
                  (or 'LOAD_START
                      'REQUEST_STARTING
                      'VARIABLE_SET
                      'LOAD_COMMIT
                      'TITLE_CHANGED
                      'LOAD_PROGRESS
                      'LOAD_FINISH
                      'SCROLL_HORIZ
                      'SCROLL_VERT)
                  _)
      (void)]
     [e
      (write e)
      (newline)]))

  (let reading ()
    (define e (read stdout))
    (unless (eof-object? e)
      (process e)
      (reading))))
