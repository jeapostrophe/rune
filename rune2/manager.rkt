#lang racket/base
(require racket/runtime-path
         racket/match
         rune2/common
         rune/lib/buffer)

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

  (uzbl-send! 'top "set inject_html = TOP")
  (send! (command:uzbl:attach 'body))
  (uzbl-send! 'body "uri http://google.com")
  (uzbl-send! 'bot "set inject_html = BOT")

  (struct state ())

  ;; xxx I want two modes: send every command to the active
  ;; application or do stuff in the minibuffer on the bottom

  ;; xxx I want the mini-buffer to be like a little edit, using the
  ;; same structures, etc as the editor program

  ;; xxx I want the mini-buffer to keep track of/have a unified
  ;; history/completion system
  (define (process b e)
    (match e
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
      b]
     [(event:rune:key (? char? c))
      (define bp (buffer-insert-char b 0 0 c))
      (uzbl-send! 'bot (format "set inject_html = ~a" (buffer->string bp)))
      bp]
     [e
      (write e)
      (newline)
      b]))

  (let reading ([b (string->buffer "")])
    (define e (read stdout))
    (unless (eof-object? e)
      (reading (process b e)))))
