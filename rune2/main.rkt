#lang racket/base
(require racket/gui/base
         racket/class
         racket/list
         racket/match
         racket/port
         racket/format
         racket/system
         rune2/socket
         racket/file
         unstable/socket
         racket/async-channel
         racket/runtime-path)

(define UZBL-PATH "/usr/bin/uzbl-core")
(define-runtime-path default-config "uzbl.config")
(define SOCKET-DIR "/tmp/rune")

(define uzbl%
  (class socket%
    (init name)
    (init-field on-size-f)

    (super-new)

    (inherit get-id)

    (define/override (on-size w h)
      (on-size-f w h))

    (define s-id (get-id))
    (printf "SPAWNING ~a\n" name)
    (define-values (sp _o stdin _e)
      (subprocess (current-output-port) #f (current-error-port)
                  UZBL-PATH
                  "-c" default-config
                  "-n" (~a name)
                  "-s" (number->string s-id)))
    (close-output-port stdin)))

;; xxx experiment with mplayer:
;; http://cpansearch.perl.org/src/GBROWN/Gtk2-Ex-MPlayerEmbed-0.02/lib/Gtk2/Ex/MPlayerEmbed.pm

(module+ main
  (define rf (new frame% [label "Rune"]))
  (define rp (new vertical-panel% [parent rf]))

  (when (directory-exists? SOCKET-DIR)
    (delete-directory/files SOCKET-DIR))
  (make-directory SOCKET-DIR)

  (define uzbl-manager
    (let ()
      (define new-uzbl-ch (make-async-channel))
      (define msg-ch (make-async-channel))
      (define to-register-ch (make-async-channel))
      (define from-register-ch (make-async-channel))

      (define registerer-t
        (thread
         (λ ()
           (define socket-dir-evt (filesystem-change-evt SOCKET-DIR))
           (let registering ([names empty])
             (define names-p
               (filter
                (λ (name)
                  (define pth (build-path SOCKET-DIR (format "uzbl_socket_~a" name)))
                  (cond
                    [(file-exists? pth)
                     (define-values (from-uzbl to-uzbl)
                       (unix-socket-connect pth))
                     (async-channel-put to-register-ch (cons name to-uzbl))
                     (async-channel-put from-register-ch (cons name from-uzbl))
                     #f]
                    [else
                     #t]))
                names))
             (sync
              (handle-evt 
               socket-dir-evt
               (λ _
                 (registering names-p)))
              (handle-evt
               new-uzbl-ch
               (λ (name)
                 (registering (cons name names-p)))))))))

      (define reader-t
        (thread
         (λ ()
           (let reading ([froms empty])
             (apply
              sync
              (handle-evt from-register-ch
                          (match-lambda
                           [(cons name from)
                            ;; xxx for some reason the config file for
                            ;; this gets ignored
                            (command name "set show_status = off")
                            (reading (cons from froms))]))
              (for/list ([from (in-list froms)])
                (handle-evt
                 (read-line-evt from)
                 (λ (l)
                   ;; xxx analyze
                   (displayln l)
                   (reading froms)))))))))

      (define msger-t
        (thread
         (λ ()
           (let msging ([name->to (hasheq)]
                        [msgs empty])
             (define msgs-p
               (filter
                (match-lambda
                 [(cons name cmd)
                  (define to-uzbl (hash-ref name->to name #f))
                  (cond
                    [to-uzbl
                     (printf "CMD: ~a\n" cmd)
                     (displayln cmd to-uzbl)
                     (flush-output to-uzbl)
                     #f]
                    [else
                     #t])])
                msgs))
             (sync
              (handle-evt
               to-register-ch
               (match-lambda
                [(cons name to)
                 (msging (hash-set name->to name to) msgs)]))
              (handle-evt
               msg-ch
               (λ (m)
                 (msging name->to (cons m msgs-p)))))))))

      (define (new-uzbl parent [name (current-seconds)])
        (define gui-obj
          (new uzbl% [parent parent]
               [on-size-f
                (λ (w h)
                  (void)
                  #;
                  (command name (format "set geometry = ~ax~a" w h)))]
               [name name]
               ;; xxx fix for body
               [min-height (if (number? name) #f 30)]
               [stretchable-height #f]))
        (async-channel-put new-uzbl-ch name)
        (λ (cmd)
          (command name cmd)))

      (define (command name cmd)
        (async-channel-put msg-ch (cons name cmd)))

      new-uzbl))

  ;; xxx make everything but body as tight in height as possible [by
  ;; putting it in a div and then reading its height via JS]
  ;; http://www.uzbl.org/wiki/fit-window
  (define uz:top-status
    (uzbl-manager rp 'top))

  ;; xxx make this like xmonad
  (define rbp (new horizontal-panel% [parent rp]))
  (define uz:body (uzbl-manager rbp))

  ;; xxx combine these into one?
  (define uz:bot-status
    (uzbl-manager rp 'bot))
  (define uz:cmd
    (uzbl-manager rp 'cmd))

  (require racket/runtime-path)
  (define-runtime-path here ".")
  (define (here-uri h)
    (format "uri ~a"
            (path->string
             (build-path here h))))

  (uz:top-status (here-uri "top.html"))
  (uz:body "uri http://google.com")
  (uz:bot-status (here-uri "bot.html"))
  (uz:cmd (here-uri "cmd.html"))

  (send rf show #t))
