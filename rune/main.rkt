#lang racket/base
(require racket/file
         racket/match
         racket/async-channel)

(struct buffer ())
(struct buffer:file (path content))

(define (path->buffer p)
  (buffer:file p (file->lines p)))
(define (buffer-max-row b)
  (max 0 (sub1 (length (buffer:file-content b)))))
(define (buffer-max-col b r)
  (max 0 (sub1 (string-length (list-ref (buffer:file-content b) r)))))

(struct cursor (row col))

(define (clamp lo x hi)
  (min (max lo x) hi))
(define (cursor-move a-c dc dr b)
  (match-define (cursor r c) a-c)
  (define nr (clamp 0 (+ r dr) (buffer-max-row b)))
  (define nc
    ;; xxx switch rows rather than clamp
    (clamp 0 (+ c dc) (buffer-max-col b nr)))
  (cursor nr nc))

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

(require racket/draw
         racket/class)

(define (rstate-render! gf rs)
  (define focused-vid
    (let ()
      (define vid (layout-focused-view (rstate-focus rs) (rstate-layout rs)))
      (define v (rstate-view rs vid))
      (define bid (view-buffer v))
      (define b (rstate-buffer rs bid))
      ;; xxx get this from an overlay?
      (set-gui-frame-label! gf (buffer:file-path b))
      vid))

  (define cursor-outline-c (make-object color% 0 0 255 1.0))
  (define frame-outline-c cursor-outline-c)
  (define frame-outline-c/dull (make-object color% 127 127 127 0.5))
  (define cursor-fill-c/focus (make-object color% 0 0 255 0.5))
  (define cursor-fill-c/unfocus (make-object color% 0 0 255 0.0))

  (define (draw! w h dc)
    ;; xxx configurable
    (send dc clear)
    (send dc set-font (make-font #:family 'modern))
    (send dc set-text-foreground "black")
    (send dc set-text-mode 'transparent)

    (define-values (char-width char-height)
      (let-values ([(width height xtra-below xtra-above)
                    (send dc get-text-extent " ")])
        (values width height)))

    (define margin% 0.5)
    (define hmargin (* char-width margin%))
    (define vmargin (* char-height margin%))

    (define (draw-layout! w h l)
      (define old (send dc get-clipping-region))
      (send dc set-clipping-rect
            0 0 w h)
      (match l
        [(vlayout vid)

         ;; xxx this notion of focused? is weird, because it is
         ;; defined as a path, but then things that weren't on that
         ;; path are focused too.
         ;;
         ;; maybe i should just merge vlayout and view?

         (define focused? (eq? focused-vid vid))
         (send dc set-pen
               (if focused?
                 frame-outline-c
                 frame-outline-c/dull)
               1 'solid)
         (send dc set-brush cursor-outline-c 'transparent)
         (send dc draw-rectangle 0 0 w h)
         (define txm (send dc get-transformation))
         (send dc translate hmargin vmargin)
         (draw-view! focused? (rstate-view rs vid))
         (send dc set-transformation txm)]
        [(llayout 'horizontal ls)
         (define lw (/ w (length ls)))
         (for ([l (in-list ls)]
               [i (in-naturals)])
           (define txm (send dc get-transformation))
           (send dc translate (* i lw) 0)
           (draw-layout! lw h l)
           (send dc set-transformation txm))]
        [(llayout 'vertical ls)
         (define lh (/ h (length ls)))
         (for ([l (in-list ls)]
               [i (in-naturals)])
           (define txm (send dc get-transformation))
           (send dc translate 0 (* i lh))
           (draw-layout! w lh l)
           (send dc set-transformation txm))])
      (send dc set-clipping-region old))

    (define (draw-view! focused? v)
      (define bid (view-buffer v))
      (define b (rstate-buffer rs bid))
      (for ([l (in-list (buffer:file-content b))]
            [row (in-naturals)])
        (send dc draw-text l 0 (* row char-height)))

      (let ()
        (match-define (cursor row col) (view-cursor v))
        (send dc set-brush (if focused?
                             cursor-fill-c/focus
                             cursor-fill-c/unfocus)
              'solid)
        (send dc set-pen cursor-outline-c 1 'solid)

        (send dc draw-rectangle
              (* col char-width) (* row char-height)
              char-width char-height)))

    (draw-layout! w h (rstate-layout rs)))

  (gui-frame-refresh! gf draw!)
  (void))

(define (start rs)
  (define ch (make-async-channel))
  (define gf (gui-frame ch))
  (rstate-loop rstate-loop ch gf rs))

;; We take loop as an argument so we can write tests that don't go
;; forever. Cute, huh?
(define (rstate-loop loop ch gf rs)
  (define (iloop rs)
    (loop loop ch gf rs))

  (rstate-render! gf rs)
  (define next-rs
    (let loop ()
      (gui-sync
       (choice-evt
        (handle-evt ch
                    (λ (ke)
                      (match ke
                        [#\q
                         (exit 0)]
                        [(or 'left 'right 'up 'down)
                         (define-values (dc dr)
                           (match ke
                             ['left (values -1 0)]
                             ['right (values +1 0)]
                             ['up (values 0 -1)]
                             ['down (values 0 +1)]))
                         (define vid
                           (layout-focused-view (rstate-focus rs) (rstate-layout rs)))
                         (define v (rstate-view rs vid))
                         (define bid (view-buffer v))
                         (define b (rstate-buffer rs bid))

                         (struct-copy
                          rstate rs
                          [views
                           (hash-set (rstate-views rs) vid
                                     (struct-copy view v
                                                  [cursor
                                                   (cursor-move (view-cursor v)
                                                                dc dr b)]))])]
                        [_
                         (loop)])))))))
  (iloop next-rs))

(module+ main
  (start
   (rstate (hasheq 0
                   (path->buffer "../TODO.org"))
           (hasheq 0
                   (view (cursor 0 0) 0)
                   1
                   (view (cursor 0 0) 0))
           (llayout 'horizontal
                    (list (vlayout 0)
                          (llayout 'vertical
                                   (list (vlayout 1)
                                         (vlayout 0)))
                          (vlayout 0)))
           '(0))))
