#lang racket/base
(require racket/gui/base
         racket/class
         racket/system
         racket/format
         racket/file
         rune2/socket
         unstable/socket
         racket/port
         racket/list
         racket/async-channel
         racket/runtime-path)

(define UZBL-PATH "/usr/bin/uzbl-core")
(define-runtime-path default-config "uzbl.config")

(define SOCKET-DIR "/tmp/rune")

(define (make-uzbl-manager)
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
               ;; xxx should analyze this
               (displayln l)
               (receiving froms)))))))))

  (define (attach-uzbl name so)
    (define s-id (send so get-id))

    (define-values (sp _o stdin _e)
      (subprocess (current-output-port) #f (current-error-port)
                  UZBL-PATH
                  "-c" default-config
                  "-n" (~a name)
                  "-s" (number->string s-id)))
    (close-output-port stdin)

    (define uzbl-socket-pth (build-path SOCKET-DIR (~a "uzbl_socket_" name)))
    (define to-ac (make-async-channel))
    (define communicator
      (thread
       (λ ()
         ;; xxx there should be a better way
         (let wait ()
           (unless (file-exists? uzbl-socket-pth)
             (sleep)
             (wait)))

         (define-values (from-uzbl to-uzbl)
           (unix-socket-connect uzbl-socket-pth))

         (async-channel-put new-from-ch from-uzbl)

         (let sending ()
           (define cmd (async-channel-get to-ac))
           (printf "CMD: ~a\n" cmd)
           (displayln cmd to-uzbl)
           (flush-output to-uzbl)
           (sending)))))

    (define (command cmd)
      (async-channel-put to-ac cmd))

    ;; xxx for some reason the config file for this gets ignored
    (command "set show_status = off")

    command)

  attach-uzbl)

;; xxx experiment with mplayer:
;; http://cpansearch.perl.org/src/GBROWN/Gtk2-Ex-MPlayerEmbed-0.02/lib/Gtk2/Ex/MPlayerEmbed.pm

(module+ main
  (when (directory-exists? SOCKET-DIR)
    (delete-directory/files SOCKET-DIR))
  (make-directory SOCKET-DIR)

  (define rf (new frame% [label "Rune"]))
  (define rp (new vertical-panel% [parent rf]))

  (define uzbl-manager (make-uzbl-manager))

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
  (define rbp (new horizontal-panel% [parent rp]))
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

  (send rf show #t))
