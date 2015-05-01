#lang racket/base
(require racket/runtime-path
         racket/match
         racket/list
         racket/file
         json
         hirune/common
         hirune/util
         hirune
         hirune/editor)

(define RACKET-PATH (find-executable-path "racket"))
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
                 racket/path
                 racket/async-channel
                 net/url)
  (define confirm-ch (make-async-channel))
  (serve
   #:confirmation-channel
   confirm-ch
   #:dispatch
   (seq:make (files:make
              #:url->path (make-url->path "/")
              #:path->mime-type
              (λ (p)
                (match (filename-extension p)
                  [#"html"
                   #"text/html; charset=UTF-8"]
                  [_
                   #f])))
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

(define (json-out jo out)
  (write-json jo out)
  (fprintf out "\r\n")
  (flush-output out))

(define (json-read-evt f p)
  (local-require racket/async-channel)
  (define ch (make-async-channel))
  (define t
    (thread
     (λ ()
       (let loop ()
         (async-channel-put ch (f (read-json p)))
         (loop)))))
  ch)

;; xxx this could be more exhaustive and tested
(define (map-json-e e)
  (match e
   [(list "key" ms k)
    (define k1 (string-ref k 0))
    (define mk
      (match k1
        [#\return
         '<return>]
        [#\rubout
         '<backspace>]
        [#\uF728
         '<delete>]
        [#\uF702
         '<left>]
        [#\uF703
         '<right>]
        [#\uF700
         '<up>]
        [#\uF701
         '<down>]
        [#\space
         '<space>]
        [#\u001B
         '<escape>]
        [#\tab
         '<tab>]
        [_
         k1]))
    (define fk
      (cond
        [(string=? "" ms)
         mk]
        [(and (string=? "S-" ms) (char? mk))
         mk]
        [else
         (string->symbol (format "~a~a" ms k))]))
    (eprintf "key: ~v -> ~v\n" e fk)
    (event:hirune:key fk)]))

(module+ main
  (require racket/tcp)
  (define-values (gui-outp gui-inp) (tcp-connect "localhost" 7331))
  (define gui-out (json-read-evt map-json-e gui-outp))

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
    (json-out
     (hasheq 'call "url"
             'view name
             'url (format "~a~a"
                          (path->hirune-file-url p)
                          after))
     gui-inp)
    (define op (hash-ref name->old name #f))
    (when op
      (delete-file op))
    (hash-set! name->old name p))

  ;; (writeln (command:uzbl:attach 'app) gui-in)
  (define repl-app (spawn-app 'mid "repl.rkt"))
  ;; (writeln (command:uzbl:attach 'app2) gui-in)
  (define repl-app2 (spawn-app 'mid2 "repl.rkt"))

  (define (refresh s)
    (match-define (manager ha edit) s)

    (uzbl-update!
     "bot"
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
     "top"
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
       (uzbl-update! (symbol->string (hiapp-name ha)) c)
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
        ;; xxx eof protect (kill app)
        (reading (command-process s c) s)))
     (handle-evt
      gui-out
      (λ (e)
        (unless (eof-object? e)
          (reading (manager-process s e) s)))))))
