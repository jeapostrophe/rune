#lang racket/base
(require racket/runtime-path
         racket/match
         racket/list
         racket/file
         hirune/common
         hirune/util
         hirune
         hirune/editor)

(define RACKET-PATH (find-executable-path "racket"))
(define-runtime-path gui-path "gui.rkt")
(define-runtime-path apps-path "apps")
(define-runtime-path domo.jpg "static/domo.jpg")

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
    (match-define (manager ha edit) s)

    (uzbl-update!
     'bot
     (hirune-file
      `(div
        (span ([class "line bghi_bg"])
              ,(format "~a"
                       ;; xxx get more information
                       (hiapp-label ha)))
        (span ([class "line"])
              ;; xxx change this with M-x vs cmd vs Input
              "⚡ "
              ,@(for/list ([c (in-sequences
                               (in-string (editor->string edit))
                               (in-string " "))]
                           [i (in-naturals)])
                  (define cs (string c))
                  (if (= i (editor-col edit))
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
       (define mbs (editor->string edit))
       (writeln (event:hirune:command mbs) (hiapp-in ha))
       (manager ha (make-editor))]
      [e
       (define editp (editor-process edit e))
       (manager ha editp)]))

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

  (let reading ([s (manager repl-app (make-editor))]
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
