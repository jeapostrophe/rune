#lang racket/base
(require racket/gui/base
         racket/class
         racket/system
         rune2/socket)

(define rune-frame%
  (class frame%
    (init-field on-size-f)

    (define/override (on-size w h)
      (on-size-f w h))

    (super-new)))

(module+ main
  (define f
    (new rune-frame%
         [label "Rune"]
         [on-size-f
          (λ (w h)
            (send s set-size w h))]))

  (define s #f)
  (set! s (new socket% [parent f]))

  (send f show #t)

  ;; xxx experiment with mplayer:
  ;; http://cpansearch.perl.org/src/GBROWN/Gtk2-Ex-MPlayerEmbed-0.02/lib/Gtk2/Ex/MPlayerEmbed.pm

  (require racket/cmdline)

  (command-line
   #:args (fst snd)
   (thread
    (λ ()
      (let loop ()
        (send s add-id (string->number fst))
        ;; get from uzbl-core -e -p
        ;; XXX why does uzbl die when rune exits?

        (read)

        (send s add-id (string->number snd))
        (read)

        (loop)))))

  (when #f
    (define id (send s get-id))
    (thread (λ () (system* "/usr/bin/uzbl-core" "-s" (number->string id))))))
