#lang racket/base
(require racket/gui/base
         racket/class
         racket/system
         rune2/socket
         unstable/socket
         racket/runtime-path)

(define UZBL-PATH "/usr/bin/uzbl-core")
(define-runtime-path default-config "uzbl.config")

(define uzbl%
  (class socket%
    (super-new)

    (inherit get-id)

    (define s-id (get-id))
    (define-values (sp _o stdin _e)
      (subprocess (current-output-port) #f (current-error-port)
                  UZBL-PATH "-c" default-config "-s" (number->string s-id)))
    (close-output-port stdin)
    
    (define pid (subprocess-pid sp))
    (define uzbl-socket-pth (build-path "/tmp" (format "uzbl_socket_~a" pid)))
    
    (let loop ()
      (unless (file-exists? uzbl-socket-pth)
        (sleep)
        (loop)))

    (define-values (from-uzbl to-uzbl)
      (unix-socket-connect uzbl-socket-pth))

    (define/public (command cmd)
      (printf "CMD: ~a\n" cmd)
      (displayln cmd to-uzbl)
      (flush-output to-uzbl))

    (define/override (on-size w h)
      ;; (command "print @{geometry}")
      (command "print @uri")
      (command "print geometry is @geometry period")
      (super on-size w h)
      (command (format "set geometry = ~ax~a" w h)))))

;; xxx experiment with mplayer:
;; http://cpansearch.perl.org/src/GBROWN/Gtk2-Ex-MPlayerEmbed-0.02/lib/Gtk2/Ex/MPlayerEmbed.pm

(module+ main
  (define rf
    (new frame% [label "Rune"]))

  (define rp
    (new vertical-panel%
         [parent rf]))

  ;; xxx make everything but body as tight in height as possible [by
  ;; putting it in a div and then reading its height via JS]
  ;; http://www.uzbl.org/wiki/fit-window
  (define uz:top-status
    (new uzbl% [parent rp]
         [min-height 30]
         [stretchable-height #f]))
  ;; xxx make this like xmonad
  (define rbp (new horizontal-panel% [parent rp]))
  (define uz:body (new uzbl% [parent rbp]))
  ;; xxx combine these into one?
  (define uz:bot-status
    (new uzbl% [parent rp]
         [min-height 30]
         [stretchable-height #f]))
  (define uz:cmd
    (new uzbl% [parent rp]
         [min-height 30]
         [stretchable-height #f]))

  (require racket/runtime-path)
  (define-runtime-path here ".")
  (define (here-uri h)
    (format "uri ~a"
            (path->string
             (build-path here h))))

  (send uz:top-status command (here-uri "top.html"))
  (send uz:body command "uri http://google.com")
  (send uz:bot-status command (here-uri "bot.html"))
  (send uz:cmd command (here-uri "cmd.html"))

  (send rf show #t))
