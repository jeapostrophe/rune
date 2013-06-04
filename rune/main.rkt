#lang racket/base
(require racket/file
         racket/match
         racket/async-channel)

(struct buffer (name content))
(struct window (cursor buffer))

(struct layout ())
(struct wlayout layout (w))
(struct hlayout layout (left right))
(struct vlayout layout (top bottom))
(struct frame (focus layout))

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

  (provide gui-frame
           set-gui-frame-label!
           gui-frame-refresh!))

(require (submod "." rune/gui/racket))

(define (layout-follow-path p l)
  (match* (p l)
    [((list) (? wlayout? l))
     (wlayout-w l)]
    [((list-rest 'left p) (? hlayout? l))
     (layout-follow-path p (hlayout-left l))]
    [((list-rest 'right p) (? hlayout? l))
     (layout-follow-path p (hlayout-right l))]
    [((list-rest 'top p) (? vlayout? l))
     (layout-follow-path p (vlayout-top l))]
    [((list-rest 'bottom p) (? vlayout? l))
     (layout-follow-path p (vlayout-bottom l))]))

(define (frame-focused-window f)
  (layout-follow-path (frame-focus f) (frame-layout f)))

(struct iframe (f gf ch))

(define (layout-render! w h dc l)
  (void))

(define (iframe-render! if)
  (define f (iframe-f if))
  (define gf (iframe-gf if))
  (local-require racket/draw
                 racket/class)
  (gui-frame-refresh!
   gf
   (λ (w h dc)
     (send dc clear)
     (send dc set-font (make-font))
     (send dc set-text-foreground "black")
     (send dc set-text-mode 'transparent)

     (send dc draw-text "Hi!!!" 0 0)

     (layout-render! w h dc (frame-layout f))))
  (void))

(define (start fs)
  (define ifs
    (for/list ([f (in-list fs)])
      ;; xxx abstract into function
      (define ch (make-async-channel))
      (define gf (gui-frame ch))
      (set-gui-frame-label! gf (buffer-name (window-buffer (frame-focused-window f))))
      (define if (iframe f gf ch))
      (iframe-render! if)
      if))

  (void))

(module+ main
  (start
   (list
    (frame '()
           (wlayout
            (window
             0
             (buffer "../TODO.org"
                     (file->string "../TODO.org"))))))))
