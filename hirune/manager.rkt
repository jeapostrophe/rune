#lang racket/base
(require racket/runtime-path
         racket/match
         racket/list
         racket/file
         racket/string
         xml
         hirune/common
         hirune/util
         hirune
         racket/class
         rune/lib/buffer
         (for-syntax racket/base))

(define RACKET-PATH (find-executable-path "racket"))
(define-runtime-path gui-path "gui.rkt")
(define-runtime-path apps-path "apps")
(define-runtime-path domo.jpg "static/domo.jpg")

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
                 web-server/dispatchers/dispatch
                 racket/async-channel
                 net/url)
  (define confirm-ch (make-async-channel))
  (serve
   #:confirmation-channel
   confirm-ch
   #:dispatch
   (seq:make (files:make #:url->path (make-url->path "/"))
             (λ (conn req)
               (eprintf "dropped req: ~a\n"
                        (url->string (request-uri req)))))
   #:port 0)
  (async-channel-get confirm-ch))

(struct manager (happ minibuf) #:transparent)
(struct hiapp (name label in out) #:transparent)
(struct editor (buf col) #:transparent)

(define (spawn-app name impl)
  (define impl-path (build-path apps-path impl))
  (define-values (sp out in _3)
    (subprocess #f #f (current-error-port)
                RACKET-PATH
                "-t" impl-path
                "--"
                "--file-port" (number->string (hirune-file-port))))
  (hiapp name impl in (read-evt out)))

(module+ main
  (define-values (sp gui-outp gui-in _3)
    (subprocess #f #f (current-error-port)
                RACKET-PATH
                "-t" gui-path))
  (define gui-out (read-evt gui-outp))

  (when (directory-exists? HIRUNE-DIR)
    (delete-directory/files HIRUNE-DIR))
  (make-directory* HIRUNE-DIR)

  (define the-hirune-file-port (start-hirune-file-server))
  ;; xxx parameterize
  (hirune-file-port the-hirune-file-port)

  (define name->old (make-hasheq))
  (define (uzbl-update! name ur)
    (match-define (command:hirune:update after pbs) ur)
    (define p (bytes->path pbs))
    (writeln
     (command:uzbl:send name
                        (format "set uri = ~a~a"
                                (path->hirune-file-url p)
                                after))
     gui-in)
    (define op (hash-ref name->old name #f))
    (when op
      (delete-file op))
    (hash-set! name->old name p))

  (writeln (command:uzbl:attach 'app) gui-in)
  (define repl-app (spawn-app 'app "repl.rkt"))

  (define (refresh s)
    (match-define (manager ha (editor mb mbc)) s)

    (uzbl-update!
     'bot
     (hirune-file
      `(div
        (span ([class "line bghi_bg"])
              ,(format "~a"
                       (hiapp-label ha)))
        (span ([class "line"])
              "⚡ "
              ,@(for/list ([c (in-sequences
                               (in-string (buffer->string mb))
                               (in-string " "))]
                           [i (in-naturals)])
                  (define cs (string c))
                  (if (= i mbc)
                    `(span ([class "cursor"]) ,cs)
                    cs))))))

    (uzbl-update!
     'top
     (hirune-file
      `(span ([class "line"])
             ,(format "昼寝(ひるね) ~a: ⊨αβγδεζηθικλμνξοπρςτυφχψω"
                      (current-milliseconds))
             (img ([style "float: right;"]
                   [src ,(path->hirune-file-url domo.jpg)]) "")))))

  (define (initial-editor)
    (editor (string->buffer "") 0))

  (define (manager-process s e)
    (match-define (manager ha edit) s)
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
      ;; xxx this kind of shows a difference between M-x and a REPL/sh
      [(event:hirune:key '<return>)
       (match-define (editor mb _) edit)
       (define mbs (buffer->string mb))
       (writeln (event:hirune:command mbs) (hiapp-in ha))
       (manager ha (initial-editor))]
      [e
       (define editp (editor-process edit e))
       (manager ha editp)]))

  (define (editor-process s e)
    (match-define (editor mb mbc) s)
    (match e
      [(event:hirune:key 'C-<left>)
       (editor mb 0)]
      [(event:hirune:key '<left>)
       (editor mb (max 0 (sub1 mbc)))]
      [(event:hirune:key 'C-<right>)
       (editor mb (buffer-row-cols mb 0))]
      [(event:hirune:key '<right>)
       (editor mb (min (buffer-row-cols mb 0) (add1 mbc)))]
      [(event:hirune:key (or '<backspace> 'S-<backspace>))
       (cond
         [(> mbc 0)
          (define-values (_ mbp) (buffer-delete-previous mb 0 mbc))
          (editor mbp (sub1 mbc))]
         [else
          s])]
      [(event:hirune:key '<delete>)
       (cond
         [(< mbc (buffer-row-cols mb 0))
          (define-values (_ mbp) (buffer-delete-next mb 0 mbc))
          (editor mbp mbc)]
         [else
          s])]
      [(event:hirune:key
        (or (? char? c)
            (and (or 'S-<space> '<space>)
                 (bind c #\space))))
       (define mbp (buffer-insert-char mb 0 mbc c))
       (editor mbp (add1 mbc))]
      [e
       (writeln `(event ,e))
       s]))

  (define (command-process s c)
    (match-define (manager ha e) s)
    (match c
      [(? command:hirune:update?)
       (uzbl-update! (hiapp-name ha) c)
       s]
      [(command:hirune:label ls)
       (define hap
         (struct-copy hiapp ha
                      [label ls]))
       (manager hap e)]
      [_
       (writeln `(command ,c))
       s]))

  (let reading ([s (manager repl-app (initial-editor))]
                [last #f])
    (match-define (manager ha _) s)
    (unless (equal? s last)
      (refresh s))
    (sync
     (handle-evt
      (hiapp-out ha)
      (λ (c)
        ;; xxx eof protect
        (reading (command-process s c) s)))
     (handle-evt
      gui-out
      (λ (e)
        (unless (eof-object? e)
          (reading (manager-process s e) s)))))))
