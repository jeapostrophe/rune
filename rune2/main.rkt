#lang racket/base
(require racket/gui/base
         racket/class
         racket/system
         racket/format
         racket/file
         rune2/socket
         racket/match
         racket/port
         racket/list
         racket/async-channel
         racket/runtime-path)

(define UZBL-PATH "/usr/bin/uzbl-core")
(define-runtime-path default-config "uzbl.config")

(define SOCKET-DIR "/tmp/rune")

(struct event ()
        #:prefab)
(struct event:uzbl event
        (instance name details)
        #:prefab)
(struct event:rune-key event
        (sym)
        #:prefab)

(define (key-event->rune-key ke)
  (define kc
    (match (send ke get-key-code)
      [#\nul #f]
      [#\rubout 'delete]
      [#\backspace 'backspace]
      [#\space 'space]
      [#\return 'return]
      [#\tab 'tab]
      [(? char? c) c]
      ['release #f]
      [(? symbol? s) s]))
  (define-syntax-rule (m b c)
    (if (send ke b) (format "~a-" c) ""))
  (define mods
    (string-append
     (m   get-shift-down "S")
     (m get-control-down "C")
     (m    get-meta-down "M")
     (m    get-mod3-down "M3")
     (m    get-mod4-down "M4")
     (m    get-mod5-down "M5")
     (m     get-alt-down "A")))
  (define mods?
    (and (not (string=? "" mods))
         (not (and (string=? "S-" mods)
                   (char? kc)))))
  (define kc-e
    (if (symbol? kc)
      (string->symbol (format "<~a>" kc))
      kc))
  (and kc-e
       (if mods?
         (string->symbol (format "~a~a" mods kc-e))
         kc-e)))

(define (filesystem-exists-evt p)
  (define signal (make-semaphore))
  ;; xxx there should be a better way
  (thread
   (λ ()
     (let wait ()
       (cond
         [(file-exists? p)
          (semaphore-post signal)]
         [else
          (sleep)
          (wait)]))))
  signal)

(define uzbl-manager%
  (class object%
    (init-field event-sink)

    (define new-from-ch (make-async-channel))
    (define receiver-t
      (thread
       (λ ()
         (let receiving ([froms empty])
           (apply
            sync
            (handle-evt
             new-from-ch
             (λ (from)
               (receiving (cons from froms))))
            (for/list ([from (in-list froms)])
              (handle-evt
               (read-line-evt from)
               (λ (l)
                 (match-define
                  (regexp #rx"^EVENT \\[([0-9A-Za-z]+)\\] ([^ ]+) ?(.*)$"
                          (list _ instance-s name-s details))
                  l)
                 (define instance (read (open-input-string instance-s)))
                 (define name (string->symbol name-s))
                 (async-channel-put event-sink (event:uzbl instance name details))
                 (receiving froms)))))))))

    (define new-to-ch (make-async-channel))
    (define to-ch (make-async-channel))
    (define sender-t
      (thread
       (λ ()
         (let sending ([name->to (hasheq)] [msgs empty])
           (define msgs-p
             (filter
              (match-lambda
               [(cons name cmd)
                (define to-uzbl (hash-ref name->to name #f))
                (cond
                  [to-uzbl
                   (displayln cmd to-uzbl)
                   (flush-output to-uzbl)
                   #f]
                  [else
                   #t])])
              msgs))
           (sync
            (handle-evt
             new-to-ch
             (match-lambda
              [(cons name to)
               (sending (hash-set name->to name to) msgs-p)]))
            (handle-evt
             to-ch
             (λ (m)
               (sending name->to (cons m msgs-p)))))))))

    (define/public (attach name so)
      (define s-id (send so get-id))

      (define-values (sp stdout stdin _e)
        (subprocess #f #f (current-error-port)
                    UZBL-PATH
                    "-c" default-config
                    "-n" (~a name)
                    "-p"
                    "-s" (number->string s-id)))
      (close-output-port stdin)

      (async-channel-put new-from-ch stdout)

      ;; xxx I could wait for the FIFO_SET event
      (define waiter-t
        (thread
         (λ ()
           (define uzbl-fifo-pth
             (build-path SOCKET-DIR (~a "uzbl_fifo_" name)))
           (sync (filesystem-exists-evt uzbl-fifo-pth))
           (define to-uzbl
             (open-output-file uzbl-fifo-pth #:exists 'append))
           (async-channel-put new-to-ch (cons name to-uzbl)))))

      ;; xxx for some reason the config file for this gets ignored
      (command name "set show_status = off"))

    (define/public (command name cmd)
      (async-channel-put to-ch (cons name cmd)))

    (super-new)))

(define rune-canvas%
  (class canvas%
    (init-field on-char-f)

    (define/override (on-char ke)
      (on-char-f ke)
      (super on-char ke))

    (super-new)))

(module+ main
  (when (directory-exists? SOCKET-DIR)
    (delete-directory/files SOCKET-DIR))
  (make-directory SOCKET-DIR)

  (define rf
    (new frame% [label "Rune"]))
  (define rp
    (new vertical-panel% [parent rf]))

  (define event-sink (make-async-channel))

  (define uzbl-manager
    (new uzbl-manager% [event-sink event-sink]))
  (define (uzbl-attach! name so)
    (send uzbl-manager attach name so))
  (define (uzbl-cmd! name cmd)
    (send uzbl-manager command name cmd))

  ;; xxx make everything but body as tight in height as possible [by
  ;; putting it in a div and then reading its height via JS]
  ;; http://www.uzbl.org/wiki/fit-window
  ;; maybe scroll_vert event would help?
  (define so:top-status
    (new socket% [parent rp]
         [min-height 30]
         [stretchable-height #f]))
  (uzbl-attach! 'top so:top-status)

  ;; xxx make this like xmonad
  (define rbp
    (new horizontal-panel% [parent rp]))
  (define so:body (new socket% [parent rbp]))
  (define cv
    (new rune-canvas% [parent so:body]
         [on-char-f
          (λ (ke)
            (define rk (key-event->rune-key ke))
            (when rk
              (async-channel-put event-sink (event:rune-key rk))))]))
  (send cv focus)
  (define body-id (current-milliseconds))
  (uzbl-attach! body-id so:body)

  (define so:bot-status
    (new socket% [parent rp]
         [min-height 30]
         [stretchable-height #f]))
  (uzbl-attach! 'bot so:bot-status)

  (send rf show #t)

  ;; core setup is over

  (require racket/runtime-path)
  (define-runtime-path here ".")
  (define (here-uri h)
    (format "uri ~a"
            (path->string
             (build-path here h))))

  (uzbl-cmd! 'top (here-uri "top.html"))
  (uzbl-cmd! body-id "uri http://google.com")
  (uzbl-cmd! 'bot (here-uri "bot.html"))

  (thread
   (λ ()
     (for ([i (in-range 100)])
       (uzbl-cmd! 'bot (format "set inject_html = ~a" i))
       (sleep 1))))

  ;; xxx make an api to tell this program what to do (send commands to
  ;; certain windows and change the layout)

  ;; xxx move some of this to another program [basically, after
  ;; changing the keycodes]
  (define (handle m)
    (match m
      ;; I'm 99% sure I don't want to do anything with these
      [(event:uzbl _
                   (or 'PTR_MOVE
                       'MOD_PRESS
                       'MOD_RELEASE
                       'KEY_RELEASE)
                   _)
       (void)]
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
      [(event:uzbl _ 'KEY_PRESS (regexp #rx"^'(.*?)' (.*?)$"
                                        (list _ mods code)))
       (local-require ffi/unsafe
                      mred/private/wx/gtk/utils
                      mred/private/wx/gtk/keycode)
       (define-gdk gdk_keyval_from_name (_fun _string -> _uint))

       (define key-code
         (cond
           [(char=? #\' (string-ref code 0))
            (cond
              [(char=? #\\ (string-ref code 1))
               #\']
              [else
               (string-ref code 1)])]
           [(string=? "space" code)
            #\space]
           [else
            (map-key-code (gdk_keyval_from_name code))]))

       (handle
        (event:rune-key
         (key-event->rune-key
          (new key-event%
               ;; NOTE: UZBL ignores META_MASK so alt-down can never appear
               [key-code (or key-code #\nul)]
               [shift-down (regexp-match #rx"Shift" mods)]
               [control-down (regexp-match #rx"Ctrl" mods)]
               [meta-down (regexp-match #rx"Mod1" mods)]
               [mod3-down (regexp-match #rx"Mod3" mods)]
               [mod4-down (regexp-match #rx"Mod4" mods)]
               [mod5-down (regexp-match #rx"Mod5" mods)]))))]
      [_
       (printf "~v\n" m)]))

  (thread
   (λ ()
     (let loop ()
       (define m (async-channel-get event-sink))
       (handle m)
       (loop))))

  (void))
