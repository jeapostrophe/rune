#lang racket/base
(require racket/file
         racket/match
         racket/async-channel)

(struct buffer ())
(struct buffer:file (path content))

(struct view (cursor buffer))

(struct layout ())
(struct vlayout layout (v))
(struct llayout layout (style children))

(struct rstate (buffers views layout focus))
(define-syntax-rule (define-rstate-lookup rstate-view rstate-views err)
  (define (rstate-view rs vid)
    (hash-ref (rstate-views rs) vid
              (λ ()
                (error 'rstate-view err vid)))))

(define-rstate-lookup rstate-view rstate-views "Unknown view ~e")
(define-rstate-lookup rstate-buffer rstate-buffers "Unknown buffer ~e")

(module rune/gui/racket racket/base
  (require racket/gui/base
           racket/async-channel
           racket/class)

  (struct gframe (f c pb))

  (define gf-canvas%
    (class canvas%
      (init-field event-ch)

      (define/override (on-char event)
        ;; xxx make my own key abstraction
        (async-channel-put event-ch (send event get-key-code)))

      (super-new)))

  (define (gui-frame ch)
    (define gf (new frame% [label ""]))
    (define paint-box (box void))
    (define c (new gf-canvas% [parent gf]
                   [style '(no-autoclear transparent)]
                   [paint-callback
                    (λ (c dc) ((unbox paint-box)
                               (send c get-width)
                               (send c get-height)
                               dc))]
                   [event-ch ch]))
    (send c focus)
    (send gf show #t)
    (gframe gf c paint-box))

  (define (set-gui-frame-label! gf l)
    (send (gframe-f gf) set-label l))

  (define (gui-frame-refresh! gf f)
    (set-box! (gframe-pb gf) f)
    (send (gframe-c gf) refresh-now))

  (define gui-sync yield)

  (provide gui-frame
           gui-sync
           set-gui-frame-label!
           gui-frame-refresh!))

(require (submod "." rune/gui/racket))

(define layout-focused-view
  (match-lambda*
   [(list (list) (vlayout v))
    v]
   [(list (list-rest this more) (llayout _ cs))
    (layout-focused-view more (list-ref cs this))]))

(define (rstate-render! gf rs)
  (local-require racket/draw
                 racket/class)

  (let ()
    (define vid (layout-focused-view (rstate-focus rs) (rstate-layout rs)))
    (define v (rstate-view rs vid))
    (define bid (view-buffer v))
    (define b (rstate-buffer rs bid))
    ;; xxx make an overlay
    (set-gui-frame-label! gf (buffer:file-path b)))

  (gui-frame-refresh!
   gf
   (λ (w h dc)
     (send dc clear)
     (send dc set-font (make-font))
     (send dc set-text-foreground "black")
     (send dc set-text-mode 'transparent)

     (send dc draw-text "Hi!!!" 0 0)))
  (void))

(define (start rs)
  (define ch (make-async-channel))
  (define gf (gui-frame ch))
  (rstate-render! gf rs)
  (rstate-loop rstate-loop ch gf rs))

;; We take loop as an argument so we can write tests that don't go
;; forever. Cute, huh?
(define (rstate-loop loop ch gf rs)
  (define (iloop rs)
    (loop loop ch gf rs))
  (gui-sync
   (handle-evt ch
               (λ (ke)
                 (eprintf "Ignored ~a\n" ke)
                 (iloop rs)))))

(module+ main
  (start
   (rstate (hasheq 0
                   (buffer:file "../TODO.org"
                                (file->string "../TODO.org")))
           (hasheq 0
                   (view 0 0))
           (vlayout 0)
           '())))
