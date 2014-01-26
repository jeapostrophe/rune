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

(struct event () #:prefab)
(struct event:uzbl event
        (instance name details) #:prefab)
(struct event:gui-keyevent event
        (shift? control? meta? alt? caps? release?
                key-code) #:prefab)

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

(define (make-uzbl-manager event-sink)
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

  (define (attach-uzbl name so)
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

    (define waiter-t
      (thread
       (λ ()
         (define uzbl-fifo-pth
           (build-path SOCKET-DIR (~a "uzbl_fifo_" name)))
         (sync (filesystem-exists-evt uzbl-fifo-pth))
         (define to-uzbl
           (open-output-file uzbl-fifo-pth #:exists 'append))
         (async-channel-put new-to-ch (cons name to-uzbl)))))

    (define (command cmd)
      (async-channel-put to-ch (cons name cmd)))

    ;; xxx for some reason the config file for this gets ignored
    (command "set show_status = off")

    command)

  attach-uzbl)

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

  (define uzbl-manager (make-uzbl-manager event-sink))

  ;; xxx make everything but body as tight in height as possible [by
  ;; putting it in a div and then reading its height via JS]
  ;; http://www.uzbl.org/wiki/fit-window
  (define so:top-status
    (new socket% [parent rp]
         [min-height 30]
         [stretchable-height #f]))
  (define uz:top-status
    (uzbl-manager 'top so:top-status))

  ;; xxx make this like xmonad
  (define rbp
    (new horizontal-panel% [parent rp]))
  (define so:body (new socket% [parent rbp]))
  (define uz:body
    (uzbl-manager (current-milliseconds) so:body))

  (define so:bot-status
    (new socket% [parent rp]
         [min-height 30]
         [stretchable-height #f]))
  (define uz:bot-status
    (uzbl-manager 'bot so:bot-status))

  (require racket/runtime-path)
  (define-runtime-path here ".")
  (define (here-uri h)
    (format "uri ~a"
            (path->string
             (build-path here h))))

  (uz:top-status (here-uri "top.html"))
  (uz:body "uri http://google.com")
  (uz:bot-status (here-uri "bot.html"))

  (thread
   (λ ()
     (for ([i (in-range 100)])
       (uz:bot-status (format "set inject_html = ~a" i))
       (sleep 1))))

  (thread
   (λ ()
     (let loop ()
       (define m (async-channel-get event-sink))
       (displayln m)
       (loop))))

  (define cv
    (new rune-canvas% [parent so:body]
         [on-char-f
          (λ (ke)
            (define release?
              (eq? 'release (send ke get-key-code)))
            (define e
              (event:gui-keyevent
               (send ke get-shift-down)
               (send ke get-control-down)
               (send ke get-meta-down)
               (send ke get-alt-down)
               (send ke get-caps-down)
               release?
               (if release?
                 (send ke get-key-release-code)
                 (send ke get-key-code))))

            (async-channel-put event-sink e))]))

  ;; xxx analyze events and write a basic key/event forwarding system

  (send rf show #t)
  (send cv focus))
