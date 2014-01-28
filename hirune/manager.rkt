#lang racket/base
(require racket/runtime-path
         racket/match
         racket/list
         racket/file
         racket/string
         xml
         hirune/common
         hirune
         racket/class
         rune/lib/buffer
         (for-syntax racket/base))

(define RACKET-PATH (find-executable-path "racket"))
(define-runtime-path gui-path "gui.rkt")
(define-runtime-path domo.jpg "domo.jpg")

(define-match-expander bind
  (syntax-rules ()
    [(_ id e)
     (app (λ (_) e) id)]))

(define (start-hirune-file-server)
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
  (async-channel-get confirm-ch))

(module+ main
  (define-values (sp stdout stdin _3)
    (subprocess #f #f (current-error-port)
                RACKET-PATH
                "-t" gui-path))

  (define (send! c)
    (write c stdin)
    (newline stdin)
    (flush-output stdin))

  (make-directory* HIRUNE-DIR)

  (define the-hirune-file-port (start-hirune-file-server))
  ;; xxx parameterize
  (hirune-file-port the-hirune-file-port)

  (define name->old (make-hasheq))
  (define (uzbl-update! name ur)
    (match-define (hirune-update after pbs) ur)
    (define p (bytes->path pbs))
    (send!
     (command:uzbl:send name
                        (format "set uri = ~a~a"
                                (path->hirune-file-url p)
                                after)))
    (define op (hash-ref name->old name #f))
    (when op
      (delete-file op))
    (hash-set! name->old name p))

  (send! (command:uzbl:attach 'app))

  (struct state (minibuf minibuf-col))

  (define (refresh s)
    (match-define (state mb mbc) s)

    (uzbl-update!
     'bot
     (hirune-file/buffer/cursor mb 0 mbc))

    (uzbl-update!
     'top
     (hirune-file
      `(span ([class "line"])
             ,(format "昼寝(ひるね) ~a: ⊨αβγδεζηθικλμνξοπρςτυφχψω"
                      (current-milliseconds))
             (img ([style "float: right;"]
                   [src ,(path->hirune-file-url domo.jpg)]) "")))))

  ;; xxx separate most of the commands out into functions to make it
  ;; easy to customize the bindings
  (define (process s e)
    (match-define (state mb mbc) s)
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
       (state mb 0)]
      [(event:hirune:key '<left>)
       (state mb (max 0 (sub1 mbc)))]
      [(event:hirune:key 'C-<right>)
       (state mb (buffer-row-cols mb 0))]
      [(event:hirune:key '<right>)
       (state mb (min (buffer-row-cols mb 0) (add1 mbc)))]
      [(event:hirune:key (or '<backspace> 'S-<backspace>))
       (cond
         [(> mbc 0)
          (define-values (_ mbp) (buffer-delete-previous mb 0 mbc))
          (state mbp (sub1 mbc))]
         [else
          s])]
      [(event:hirune:key '<delete>)
       (cond
         [(< mbc (buffer-row-cols mb 0))
          (define-values (_ mbp) (buffer-delete-next mb 0 mbc))
          (state mbp mbc)]
         [else
          s])]
      [(event:hirune:key
        (or (? char? c)
            (and (or 'S-<space> '<space>)
                 (bind c #\space))))
       (define mbp (buffer-insert-char mb 0 mbc c))
       (state mbp (add1 mbc))]
      [(event:hirune:key '<return>)
       (define mbs (buffer->string mb))
       ;; xxx send this to active thing
       (define mbp (string->buffer ""))

       (state mbp 0)]
      [e
       (write e)
       (newline)
       s]))

  (let reading ([s (state (string->buffer "") 0)] [last #f])
    (unless (eq? s last)
      (refresh s))
    (define e (read stdout))
    (unless (eof-object? e)
      (reading (process s e) s))))
