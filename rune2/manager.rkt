#lang racket/base
(require racket/runtime-path
         racket/match
         racket/list
         racket/file
         racket/string
         xml
         rune2/common
         racket/class
         rune/lib/buffer
         (for-syntax racket/base))

(define RACKET-PATH (find-executable-path "racket"))
(define-runtime-path gui-path "gui.rkt")
(define-runtime-path rune-file.css "rune-file.css")
(define-runtime-path rune-file.js "rune-file.js")
(define-runtime-path domo.jpg "domo.jpg")

(define-match-expander bind
  (syntax-rules ()
    [(_ id e)
     (app (λ (_) e) id)]))

(module+ test
  (match 1
    [(and 1 (bind x 3)) x]
    [(bind x 2) x]))

(define (snoc l x)
  (append l (list x)))

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

  (define rune-file-port
    (let ()
      (local-require web-server/web-server
                     web-server/http
                     (prefix-in files: web-server/dispatchers/dispatch-files)
                     (prefix-in seq: web-server/dispatchers/dispatch-sequencer)
                     (prefix-in lift: web-server/dispatchers/dispatch-lift)
                     web-server/dispatchers/filesystem-map
                     racket/async-channel)
      (define confirm-ch (make-async-channel))
      (serve
       #:confirmation-channel
       confirm-ch
       #:dispatch
       (files:make #:url->path (make-url->path "/"))
       #:port 0)
      (async-channel-get confirm-ch)))

  (define (path->rune-file-url p)
    (format "http://localhost:~a~a" rune-file-port p))

  ;; xxx delete old files

  (define rune-file%
    (class object%
      (define p #f)

      (define (really-uri name after)
        (unless p
          (error 'rune-file% "Initialize first!"))
        (uzbl-send! name (format "set uri = ~a#~a"
                                 (path->rune-file-url p)
                                 after)))
      (define/public (uri name)
        (really-uri name ""))
      ;; xxx add bot, center, and top
      ;; xxx add cursor, or not
      (define/public (uri/bot name row col)
        (really-uri name
                    (format "row~a" row)))
      (define/public (rep ls)
        (define xe
          `(html
            (head
             (link ([rel "stylesheet"]
                    [type "text/css"]
                    [href ,(path->rune-file-url rune-file.css)]))
             (script ([src "//code.jquery.com/jquery-1.10.1.min.js"]) "")
             (script ([src ,(path->rune-file-url rune-file.js)]) ""))
            (body
             (div ([id "top"] [class "file"])
                  ,@(for/list ([r (in-list ls)]
                               [i (in-naturals)])
                      ;; xxx show the cursor
                      `(span ([class "row"] [id ,(format "row~a" i)])
                             ,r))))))
        (define np (make-temporary-file "rune-~a.html"))
        (with-output-to-file np
          #:exists 'replace
          (λ () (write-xexpr xe)))
        (set! p np))
      (define/public (bufrep b)
        (rep (buffer->strings b)))
      (define/public (bufrep/cursor b row col)
        (bufrep (buffer-insert-char b row col #\‸)))

      (super-new)

      (rep empty)))

  (send! (command:uzbl:attach 'body))

  (require racket/sandbox)
  (define evaler
    (parameterize ([sandbox-memory-limit #f]
                   [sandbox-output 'string]
                   [sandbox-error-output 'string])
      (make-evaluator 'racket '(require math))))

  (define top-rf (new rune-file%))
  (define history-rf (new rune-file%))
  (define minibuf-rf (new rune-file%))

  (send top-rf uri 'top)
  (send history-rf uri/bot 'body 0 0)
  (send minibuf-rf uri/bot 'bot 0 0)

  (struct state (history minibuf minibuf-cols))
  (struct history (cmd stdout stderr))
  (define is
    (state empty (string->buffer "") 0))

  ;; xxx I want two modes: send every key/command to the active
  ;; application or do stuff in the minibuffer on the bottom

  ;; xxx I want the mini-buffer to be like a little edit, using the
  ;; same structures, etc as the editor program

  ;; xxx I want the mini-buffer to keep track of/have a unified
  ;; history/completion system

  (define (refresh s)
    (match-define (state h mb mbc) s)

    (send minibuf-rf bufrep/cursor mb 0 mbc)
    (send minibuf-rf uri/bot 'bot 0 mbc)

    (send history-rf rep
          (for/list ([h (in-list h)])
            (match-define (history c o e) h)
            `(span (span ([class "row"]) "> " ,c)
                   ,@(for/list ([o (in-list o)])
                       `(span ([class "row cyan"]) ,o))
                   ,@(for/list ([e (in-list e)])
                       `(span ([class "row red"]) ,e)))))
    (send history-rf uri/bot 'body (length h) 0)

    ;; xxx something more interesting
    (send top-rf rep
          (list
           `(span
             ,(format "昼寝(ひるね) ~a: ⊨αβγδεζηθικλμνξοπρςτυφχψω"
                      (current-milliseconds))
             (img ([style "float: right;"]
                   [src ,(path->rune-file-url domo.jpg)]) ""))))
    (send top-rf uri 'top))

  (define (process s e)
    (match-define (state h mb mbc) s)
    (define sp
      (match e
        ;; I'm 70% sure I don't want these
        [(and #f
              (event:uzbl (or 'bot 'top)
                          (or 'LOAD_START
                              'REQUEST_STARTING
                              'VARIABLE_SET
                              'LOAD_COMMIT
                              'TITLE_CHANGED
                              'LOAD_PROGRESS
                              'LOAD_FINISH
                              'SCROLL_HORIZ
                              'SCROLL_VERT)
                          _))
         s]
        [(event:rune:key '<left>)
         (state h mb (max 0 (sub1 mbc)))]
        [(event:rune:key '<right>)
         (state h mb (min (buffer-row-cols mb 0) (add1 mbc)))]
        [(event:rune:key '<backspace>)
         (cond
           [(> mbc 0)
            (define-values (_ mbp) (buffer-delete-previous mb 0 mbc))
            (state h mbp (sub1 mbc))]
           [else
            s])]
        [(event:rune:key '<delete>)
         (cond
           [(< mbc (buffer-row-cols mb 0))
            (define-values (_ mbp) (buffer-delete-next mb 0 mbc))
            (state h mbp mbc)]
           [else
            s])]
        [(event:rune:key
          (or (? char? c)
              (and (or 'S-<space> '<space>)
                   (bind c #\space))))
         (define mbp (buffer-insert-char mb 0 mbc c))
         (state h mbp (add1 mbc))]
        [(event:rune:key '<return>)
         (define mbs (buffer->string mb))
         (define maybe-error
           (with-handlers ([exn:fail?
                            (λ (x)
                              (string-append (exn-message x) "\n"))])
             (evaler mbs)
             ""))
         (define hp
           (snoc
            h
            (history mbs
                     (string-split
                      (get-output evaler)
                      "\n")
                     (string-split
                      (string-append (get-error-output evaler)
                                     maybe-error)
                      "\n"))))
         (define mbp (string->buffer ""))

         (state hp mbp 0)]
        [e
         (write e)
         (newline)
         s]))
    (unless (eq? s sp)
      (refresh sp))
    sp)

  (refresh is)
  (let reading ([s is])
    (define e (read stdout))
    (unless (eof-object? e)
      (reading (process s e)))))
