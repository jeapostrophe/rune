#lang racket/base
(require racket/file
         racket/list
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
  (define maybe-nc (+ c dc))
  (cond
    [(< maybe-nc 0)
     ;; Move back one row and to the far right
     (define nnr (max 0 (sub1 nr)))
     (cursor nnr (buffer-max-col b nnr))]
    [(< (buffer-max-col b nr) maybe-nc)
     (define nnr (min (add1 nr) (buffer-max-row b)))
     (cursor nnr 0)]
    [else
     (cursor nr maybe-nc)]))

(struct layout ())
(struct vlayout layout (cursor buffer))
(struct llayout layout (style children))

(struct rstate (buffers layout focus))
(define-syntax-rule (define-rstate-lookup rstate-view rstate-views err)
  (define (rstate-view rs vid)
    (hash-ref (rstate-views rs) vid
              (λ ()
                (error 'rstate-view err vid)))))

(define-rstate-lookup rstate-buffer rstate-buffers "Unknown buffer ~e")

(module rune/gui/racket racket/base
  (require racket/gui/base
           racket/async-channel
           racket/match
           racket/class)

  (struct gframe (f c pb))

  (define (key-event->emacs-key ke)
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
      (format "~a~a~a"
              (m get-control-down "C")
              (m    get-meta-down "M")
              (m   get-shift-down "S")))
    (define mods?
      (not (string=? "" mods)))
    (define kc-e
      (if (symbol? kc)
        (string->symbol (format "<~a>" kc))
        kc))
    (and kc-e
         (if mods?
           (string->symbol (format "~a~a" mods kc-e))
           kc-e)))

  (define gf-canvas%
    (class canvas%
      (init-field event-ch)

      (define/override (on-char event)
        (define ek (key-event->emacs-key event))
        (when ek
          (async-channel-put event-ch ek)))

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

  (define (set-gui-frame-label! gf i [prepend? #f])
    (define l
      (if prepend?
        (format "~a: ~a" i (send (gframe-f gf) get-label))
        i))
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

(define (focus-path-rest fp i)
  (and fp (= i (first fp)) (rest fp)))

(require racket/draw
         racket/class)

(define buffer->bm (make-hasheq))

(define (rstate-render! gf rs)
  (define cursor-outline-c (make-object color% 0 0 255 1.0))
  (define frame-outline-c cursor-outline-c)
  (define frame-outline-c/dull (make-object color% 127 127 127 0.5))
  (define cursor-fill-c/focus (make-object color% 0 0 255 0.5))
  (define cursor-fill-c/unfocus (make-object color% 0 0 255 0.0))

  (define (draw! w h dc)
    ;; xxx configurable
    (send dc clear)
    (define the-font (make-font #:family 'modern))
    (send dc set-font the-font)
    (send dc set-text-foreground "black")
    (send dc set-text-mode 'transparent)

    (define-values (char-width char-height)
      (let-values ([(width height xtra-below xtra-above)
                    (send dc get-text-extent " ")])
        (values width height)))

    (define margin% 0.5)
    (define hmargin (* char-width margin%))
    (define vmargin (* char-height margin%))

    (define (draw-layout! x y w h fp l)
      (match l
        [(vlayout c bid)
         (define focused? (empty? fp))

         ;; Render a buffer
         (let ()
           (define b (rstate-buffer rs bid))
           (when focused?
             ;; xxx get this from an overlay?
             (set-gui-frame-label! gf (buffer:file-path b)))

           (define b-bm
             (hash-ref! buffer->bm bid
                        (λ ()
                          (local-require (only-in racket/gui/base make-screen-bitmap))

                          (define max-row (buffer-max-row b))
                          (define max-col
                            (for/fold ([mc 0])
                                ([r (in-range (add1 max-row))])
                              (max mc (buffer-max-col b r))))

                          (define bm
                            (make-screen-bitmap
                             (inexact->exact (ceiling (* max-col char-width)))
                             (inexact->exact (ceiling (* max-row char-height)))))
                          (define bm-dc
                            (send bm make-dc))

                          (send bm-dc set-font the-font)

                          (for ([l (in-list (buffer:file-content b))]
                                [row (in-naturals)])
                            (send bm-dc draw-text l 0 (* row char-height)))

                          bm)))

           (send dc draw-bitmap-section b-bm
                 (+ x hmargin) (+ y vmargin)
                 0 0
                 (max 0 (- w hmargin)) (max 0 (- h vmargin))))

         ;; Outline
         (let ()
           (send dc set-pen
                 (if focused?
                   frame-outline-c
                   frame-outline-c/dull)
                 2 'solid)
           (send dc set-brush cursor-outline-c 'transparent)
           (send dc draw-rectangle x y w h))

         ;; Cursor
         (let ()
           (match-define (cursor row col) c)
           (send dc set-brush (if focused?
                                cursor-fill-c/focus
                                cursor-fill-c/unfocus)
                 'solid)
           (send dc set-pen cursor-outline-c 1 'solid)

           (let ()
             (define cursor-x (+ x hmargin (* col char-width)))
             (define cursor-y (+ y vmargin (* row char-height)))
             (unless (or (< w (+ cursor-x char-width))
                         (< h (+ cursor-y char-height)))
               (send dc draw-rectangle
                     cursor-x cursor-y
                     char-width char-height))))]
        [(llayout 'horizontal ls)
         (define lw (/ w (length ls)))
         (for ([l (in-list ls)]
               [i (in-naturals)])
           (draw-layout! (+ x (* i lw)) y lw h (focus-path-rest fp i) l))]
        [(llayout 'vertical ls)
         (define lh (/ h (length ls)))
         (for ([l (in-list ls)]
               [i (in-naturals)])
           (draw-layout! x (+ y (* i lh)) w lh (focus-path-rest fp i) l))]))

    (draw-layout! 0 0 w h (rstate-focus rs) (rstate-layout rs)))

  (gui-frame-refresh! gf draw!)
  (void))

(define (start rs)
  (define ch (make-async-channel))
  (define gf (gui-frame ch))
  (rstate-loop rstate-loop ch gf rs))

(define (list-update l i f)
  (for/list ([e (in-list l)]
             [j (in-naturals)])
    (if (= i j)
      (f e)
      e)))

(define (update-focused-layout fp l k)
  (match fp
    [(list)
     (k l)]
    [(list-rest this more)
     (struct-copy
      llayout l
      [children
       (list-update (llayout-children l)
                    this
                    (λ (old)
                      (update-focused-layout more old k)))])]))

(define (focused-layout fp l)
  (match fp
    [(list)
     l]
    [(list-rest this more)
     (focused-layout more (list-ref (llayout-children l)))]))

;; We take loop as an argument so we can write tests that don't go
;; forever. Cute, huh?
(define (rstate-loop loop ch gf rs)
  (define (iloop rs)
    (loop loop ch gf rs))

  (define before-render (current-inexact-milliseconds))
  (rstate-render! gf rs)
  (define after-render (current-inexact-milliseconds))
  (set-gui-frame-label!
   gf (format "~ams" (real->decimal-string (- after-render before-render)))
   #t)
  (define next-rs
    (let loop ()
      (gui-sync
       (choice-evt
        (handle-evt ch
                    (λ (ke)
                      (match ke
                        ['C-q
                         (exit 0)]
                        [(or '<left> '<right> '<up> '<down>)
                         (define-values (dc dr)
                           (match ke
                             ['<left> (values -1 0)]
                             ['<right> (values +1 0)]
                             ['<up> (values 0 -1)]
                             ['<down> (values 0 +1)]))

                         (struct-copy
                          rstate rs
                          [layout
                           (update-focused-layout
                            (rstate-focus rs)
                            (rstate-layout rs)
                            (λ (v)
                              (define bid (vlayout-buffer v))
                              (define b (rstate-buffer rs bid))
                              (struct-copy
                               vlayout v
                               [cursor
                                (cursor-move (vlayout-cursor v)
                                             dc dr b)])))])]
                        [x
                         (eprintf "ignored ~s\n" x)
                         (loop)])))))))
  (iloop next-rs))

(module+ main
  (start
   (rstate (hasheq 0
                   (path->buffer "../TODO.org"))
           (llayout 'horizontal
                    (list (vlayout (cursor 0 0) 0)
                          (llayout 'vertical
                                   (list (vlayout (cursor 0 0) 0)
                                         (vlayout (cursor 0 0) 0)))
                          (vlayout (cursor 0 0) 0)))
           '(0))))
