#lang racket/base
(require racket/gui/base
         racket/class
         racket/system
         rune2/socket)

(define UZBL-PATH "/usr/bin/uzbl-core")

(define uzbl%
  (class socket%
    (super-new)

    (inherit get-id)

    (define s-id (get-id))
    (define-values (sp stdout stdin stderr)
      (subprocess #f #f #f
                  UZBL-PATH "-c" "-" "-s" (number->string s-id)))

    (define/public (command cmd)
      (displayln cmd stdin)
      (flush-output stdin))

    (command "set show_status = off")))

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
