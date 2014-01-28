#lang racket/base
(require racket/runtime-path
         racket/match
         racket/list
         racket/file
         racket/string
         xml
         hirune/common
         racket/class
         rune/lib/buffer
         (for-syntax racket/base))

(define RACKET-PATH (find-executable-path "racket"))
(define-runtime-path gui-path "gui.rkt")
(define-runtime-path hirune.css "hirune.css")
(define-runtime-path hirune.js "hirune.js")
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

  (define hirune-file-port
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

  (define (path->hirune-file-url p)
    (format "http://localhost:~a~a" hirune-file-port p))

  (define hirune-file%
    (class object%
      (define p #f)

      (define (really-uri name after)
        (unless p
          (error 'hirune-file% "Initialize first!"))
        (uzbl-send! name (format "set uri = ~a#~a"
                                 (path->hirune-file-url p)
                                 after)))
      (define/public (uri name)
        (really-uri name ""))
      (define/public (uri/bot name row col)
        (really-uri name
                    (format "row~a" row)))
      (define/public (rep ls)
        (define xe
          `(html
            (head
             (link ([rel "stylesheet"]
                    [type "text/css"]
                    [href ,(path->hirune-file-url hirune.css)]))
             (script ([src "//code.jquery.com/jquery-1.10.1.min.js"]) "")
             (script ([src ,(path->hirune-file-url hirune.js)]) ""))
            (body ([class "content1 bg_bg"])
             (div ()
                  ,@(for/list ([r (in-list ls)]
                               [i (in-naturals)])
                      `(span ([class "line"] [id ,(format "line~a" i)])
                             ,r))))))
        (define np (make-temporary-file "hirune-~a.html"))
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

  (define top-rf (new hirune-file%))
  (define history-rf (new hirune-file%))
  (define minibuf-rf (new hirune-file%))

  (send top-rf uri 'top)
  (send history-rf uri/bot 'body 0 0)
  (send minibuf-rf uri/bot 'bot 0 0)

  (struct state (minibuf minibuf-col))
  (struct history (cmd stdout stderr))
  (define is
    (state empty (string->buffer "") 0))

  (define (refresh s)
    (match-define (state h mb mbc) s)

    (send minibuf-rf bufrep/cursor mb 0 mbc)
    (send minibuf-rf uri/bot 'bot 0 mbc)

    (send history-rf rep
          (for/list ([h (in-list h)])
            (match-define (history c o e) h)
            `(span (span ([class "line"]) "> " ,c)
                   ,@(for/list ([o (in-list o)])
                       `(span ([class "line cyan_fg"]) ,o))
                   ,@(for/list ([e (in-list e)])
                       `(span ([class "line red_fg"]) ,e)))))
    (send history-rf uri/bot 'body (length h) 0)

    (send top-rf rep
          (list
           `(span
             ,(format "昼寝(ひるね) ~a: ⊨αβγδεζηθικλμνξοπρςτυφχψω"
                      (current-milliseconds))
             (img ([style "float: right;"]
                   [src ,(path->hirune-file-url domo.jpg)]) ""))))
    (send top-rf uri 'top))

  ;; xxx separate most of the commands out into functions to make it
  ;; easy to customize the bindings
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
        [(event:hirune:key 'C-<left>)
         (state h mb 0)]
        [(event:hirune:key '<left>)
         (state h mb (max 0 (sub1 mbc)))]
        [(event:hirune:key 'C-<right>)
         (state h mb (buffer-row-cols mb 0))]
        [(event:hirune:key '<right>)
         (state h mb (min (buffer-row-cols mb 0) (add1 mbc)))]
        [(event:hirune:key (or '<backspace> 'S-<backspace>))
         (cond
           [(> mbc 0)
            (define-values (_ mbp) (buffer-delete-previous mb 0 mbc))
            (state h mbp (sub1 mbc))]
           [else
            s])]
        [(event:hirune:key '<delete>)
         (cond
           [(< mbc (buffer-row-cols mb 0))
            (define-values (_ mbp) (buffer-delete-next mb 0 mbc))
            (state h mbp mbc)]
           [else
            s])]
        [(event:hirune:key
          (or (? char? c)
              (and (or 'S-<space> '<space>)
                   (bind c #\space))))
         (define mbp (buffer-insert-char mb 0 mbc c))
         (state h mbp (add1 mbc))]
        [(event:hirune:key '<return>)
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
