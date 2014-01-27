#lang racket/base
(require racket/runtime-path
         racket/match
         racket/file
         racket/string
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

  (send! (command:uzbl:attach 'body))
  (uzbl-send! 'bot "set inject_html = BOT")

  (require racket/sandbox)
  (define evaler
    (parameterize ([sandbox-memory-limit #f]
                   [sandbox-output 'string]
                   [sandbox-error-output 'string])
      (make-evaluator 'racket '(require math))))
  (struct state (history history-rows minibuf minibuf-cols))
  (define is
    (state (string->buffer "") 0
           (string->buffer "") 0))

  (define (update-top!)
    ;; xxx something more interesting
    (uzbl-send! 'top (format "set inject_html = ~a" (current-milliseconds))))

  ;; xxx I want two modes: send every command to the active
  ;; application or do stuff in the minibuffer on the bottom

  ;; xxx I want the mini-buffer to be like a little edit, using the
  ;; same structures, etc as the editor program

  ;; xxx I want the mini-buffer to keep track of/have a unified
  ;; history/completion system

  (define (insert-char s c)
    (update-top!)
    (match-define (state h hr mb mbc) s)
    (define mbp (buffer-insert-char mb 0 mbc c))
    ;; xxx better formatting
    (uzbl-send! 'bot (format "set inject_html = ~a" (buffer->string mbp)))
    (state h hr mbp (add1 mbc)))

  (define history-p (make-temporary-file))

  (define (process s e)
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
       s]
      [(event:rune:key (? char? c))
       (insert-char s c)]
      [(event:rune:key '<space>)
       (insert-char s #\space)]
      [(event:rune:key '<return>)
       (update-top!)
       (match-define (state h hr mb mbc) s)
       (define mbs (buffer->string mb))
       (with-handlers ([exn:fail? void])
         (evaler mbs))
       (define addl
         (string-append
          "> "
          mbs
          "\n"
          (string-trim
           (get-output evaler))
          "\n"))
       (define hp
         (buffer-insert-string h hr 0
                               addl))
       (update-top!)
       (display-to-file (buffer->string hp) history-p #:exists 'replace)
       ;; xxx seek to new line
       ;; xxx show errors
       ;; xxx better formating
       (uzbl-send! 'body (format "uri ~a" history-p))
       (uzbl-send! 'bot (format "set inject_html = ~a" ""))

       (state hp (sub1 (buffer-rows hp)) (string->buffer "") 0)]
      [e
       (write e)
       (newline)
       s]))

  (let reading ([s is])
    (define e (read stdout))
    (unless (eof-object? e)
      (reading (process s e)))))
