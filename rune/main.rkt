#lang racket/base
(require racket/file
         racket/list
         racket/match
         racket/async-channel
         (prefix-in o: rune/lib/overlay2)
         (prefix-in z: rune/lib/buffer))

(struct buffer ())
(struct buffer:file (path [content #:mutable]))

(define (path->buffer p)
  (buffer:file p (z:string->buffer (file->string p))))
(define (buffer-max-row b)
  (z:buffer-rows (buffer:file-content b)))
(define (buffer-max-col b r)
  (z:buffer-row-cols (buffer:file-content b) r))
(define (buffer-line b r)
  (z:buffer-row (buffer:file-content b) r))
(define (buffer-insert-at! b row col c)
  (define zb (buffer:file-content b))
  (define nzb (z:buffer-insert-char zb row col c))
  (set-buffer:file-content! b nzb)
  (void))
(define (buffer-delete-next! b row col)
  (define zb (buffer:file-content b))
  (with-handlers ([exn:fail? (λ (x) #f)])
    (define-values (_ nzb) (z:buffer-delete-next zb row col))
    (set-buffer:file-content! b nzb)
    #t))
(define (buffer-delete-previous! b row col)
  (define zb (buffer:file-content b))
  (with-handlers ([exn:fail? (λ (x) #f)])
    (define-values (_ nzb) (z:buffer-delete-previous zb row col))
    (set-buffer:file-content! b nzb)
    #t))

(struct cursor (row col) #:transparent)

(define (clamp lo x hi)
  (min (max lo x) hi))
(define (cursor-move a-c dc dr b)
  (match-define (cursor r c) a-c)
  (cond
    [(zero? dr)
     (define maybe-nc (+ c dc))
     (cond
       [(< maybe-nc 0)
        ;; Move back one row and to the far right
        (define nr (max 0 (sub1 r)))
        (cursor nr (buffer-max-col b nr))]
       [(< (buffer-max-col b r) maybe-nc)
        (define nr (min (add1 r) (buffer-max-row b)))
        (cursor nr 0)]
       [else
        (cursor r maybe-nc)])]
    [(zero? dc)
     (define nr (clamp 0 (+ r dr) (buffer-max-row b)))
     (cursor nr (clamp 0 c (buffer-max-col b nr)))]))

(struct view (cursor buffer))

(struct ctxt ())
(struct ctxt:top ctxt ())

;; parent is a ctxt
;; style is a layout style
;; left is a list of focus (reversed)
;; right is a list of focus
(struct ctxt:layer ctxt (parent style left right))

;; ctxt is a ctxt
;; view is a view
(struct focus (ctxt view))

(struct rstate (buffers focus))
(define-syntax-rule (define-rstate-lookup rstate-view rstate-views err)
  (define (rstate-view rs vid)
    (hash-ref (rstate-views rs) vid
              (λ ()
                (error 'rstate-view err vid)))))

(define-rstate-lookup rstate-buffer rstate-buffers "Unknown buffer ~e")

(require rune/gui/racket)

(require racket/draw
         racket/class)

(define buffer->bm (make-hasheq))
;; xxx hack
(define buffer->os (make-hasheq))

(define (rstate-render! gf rs)
  (define cursor-outline-c (make-object color% 0 0 255 1.0))
  (define frame-outline-c cursor-outline-c)
  (define frame-outline-c/dull (make-object color% 127 127 127 0.5))
  (define cursor-fill-c/focus (make-object color% 0 0 255 0.5))
  (define cursor-fill-c/unfocus (make-object color% 0 0 255 0.0))

  (define (draw! full-w full-h dc)
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

    (define (draw-view! x y w h focused? v)
      (match-define (view c bid) v)

      ;; Render a buffer
      (let ()
        (define b (rstate-buffer rs bid))
        (when focused?
          ;; xxx get this from an overlay?
          (set-gui-frame-label! gf (buffer:file-path b)))

        (define b-bm
          (hash-ref!
           buffer->bm bid
           (λ ()
             (local-require (only-in racket/gui/base make-screen-bitmap))

             (define max-row (buffer-max-row b))
             (define max-col
               (for/fold ([mc 0])
                   ([r (in-range max-row)])
                 (max mc (buffer-max-col b r))))

             (define overlays
               (hash-ref!
                buffer->os bid
                (λ ()
                  (for/fold ([odb o:empty-odb])
                      ([row (in-range max-row)])
                    (for/fold ([odb odb])
                        ([col (in-range (buffer-max-col b row))])
                      (if (zero? (random 2))
                        (o:odb-set odb (o:rect:point bid row col) 'highlight? #t)
                        odb))))))

             (define bm
               (make-screen-bitmap
                (inexact->exact (ceiling (* max-col char-width)))
                (inexact->exact (ceiling (* max-row char-height)))))
             (define bm-dc
               (send bm make-dc))

             (send bm-dc set-font the-font)

             (for ([row (in-range max-row)])
               (define l (buffer-line b row))

               ;; xxx line at once: 178ms
               ;; (send bm-dc draw-text l 0 (* row char-height))

               (for ([char (in-string l)]
                     [col (in-naturals)])

                 (define os
                   (if #t
                     ;; slow (list): 7000ms
                     ;; fast (hash): 200-500 ms
                     (o:odb-hash overlays (o:rect:point bid row col))
                     ;; fake:  600ms
                     (hasheq 'highlight? (if (zero? (random 2)) #t #f))))
                 (send bm-dc set-text-foreground
                       (if (hash-ref os 'highlight? #f)
                         "red"
                         "black"))
                 (send bm-dc draw-text (string char)
                       (* col char-width) (* row char-height))))

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
        ;; xxx not showing for non-focused
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
                  char-width char-height)))))

    (define (draw-ctxt! x y w h c)
      (match c
        [(ctxt:top)
         (values x y w h)]
        [(ctxt:layer p s l r)
         (define-values (mx my mw mh) (draw-ctxt! x y w h p))
         (match s
           ;; xxx merge these
           ['horizontal
            (define i (length l))
            (define ew (/ mw (+ i 1 (length r))))
            (define (xof j)
              (+ mx (* j ew)))

            (for ([sf (in-list l)]
                  [rj (in-naturals 1)])
              (define j (- i rj))
              (draw-focus! (xof j) my ew mh #f sf))
            (for ([sf (in-list r)]
                  [aj (in-naturals 1)])
              (define j (+ i aj))
              (draw-focus! (xof j) my ew mh #f sf))

            (values (xof i) my ew mh)]
           ['vertical
            (define i (length l))
            (define eh (/ mh (+ i 1 (length r))))
            (define (yof j)
              (+ my (* j eh)))

            (for ([sf (in-list l)]
                  [rj (in-naturals 1)])
              (define j (- i rj))
              (draw-focus! mx (yof j) mw eh #f sf))
            (for ([sf (in-list r)]
                  [aj (in-naturals 1)])
              (define j (+ i aj))
              (draw-focus! mx (yof j) mw eh #f sf))

            (values mx (yof i) mw eh)])]))

    (define (draw-focus! x y w h focused? f)
      (match-define (focus c v) f)
      (define-values (mx my mw mh) (draw-ctxt! x y w h c))
      (draw-view! mx my mw mh focused? v))

    (draw-focus! 0 0 full-w full-h #t (rstate-focus rs)))

  (gui-frame-refresh! gf draw!)
  (void))

(define (start rs)
  (define ch (make-async-channel))
  (define gf (gui-frame ch))
  (rstate-loop rstate-loop ch gf rs))

(define-match-expander rune-key
  (syntax-rules ()
    [(_ e ...) (list* e ... _)]))

;; We take loop as an argument so we can write tests that don't go
;; forever. Cute, huh?
(define (rstate-loop loop ch gf rs)
  (let ()
    (define before-render (current-inexact-milliseconds))
    (rstate-render! gf rs)
    (define after-render (current-inexact-milliseconds))
    (set-gui-frame-label!
     gf (format "~ams" (real->decimal-string (- after-render before-render)))
     #t))

  (define (move-cursor dc dr)
    (struct-copy
     rstate rs
     [focus
      (let ()
        (match-define (focus ctxt v) (rstate-focus rs))
        (match-define (view c bid) v)
        (define b (rstate-buffer rs bid))
        (focus ctxt (view (cursor-move c dc dr b) bid)))]))

  (define (list-move-focus l r f df)
    (cond
      [(and (not (empty? l)) (negative? df))
       (values (rest l) (list* f r) (first l))]
      [(and (not (empty? r)) (positive? df))
       (values (list* f l) (rest r) (first r))]
      [else
       (values l r f)]))

  (define (ctxt-reparent np c)
    (match c
      [(ctxt:top)
       np]
      [(ctxt:layer p s l r)
       (ctxt:layer (ctxt-reparent np p) s l r)]))

  (define (move-focus df)
    (struct-copy
     rstate rs
     [focus
      (match (rstate-focus rs)
        [(focus (ctxt:layer p s l r) v)
         (define f (focus (ctxt:top) v))
         (define-values (nl nr nf) (list-move-focus l r f df))
         (match-define (focus nctxt nv) nf)
         (focus (ctxt-reparent (ctxt:layer p s nl nr) nctxt) nv)]
        [f
         f])]))

  (define (move-meta-focus df)
    (struct-copy
     rstate rs
     [focus
      (match (rstate-focus rs)
        [(focus (ctxt:layer (ctxt:layer pp ps pl pr) s l r) v)
         (define pf (focus (ctxt:layer (ctxt:top) s l r) v))
         (define-values (npl npr npf) (list-move-focus pl pr pf df))
         (match-define (focus nctxt nv) npf)
         (focus (ctxt-reparent (ctxt:layer pp ps npl npr) nctxt) nv)]
        [f
         f])]))

  (define (do-to-buffer f)
    (match-define (focus ctxt v) (rstate-focus rs))
    (match-define (view (cursor row col) bid) v)
    (define b (rstate-buffer rs bid))
    (begin0 (f b row col)
            (hash-remove! buffer->bm bid)))

  (define (insert-char c)
    (do-to-buffer
     (λ (b row col)
       (buffer-insert-at! b row col c)
       (move-cursor +1 0))))
  (define (delete-next)
    (do-to-buffer
     (λ (b row col)
       (buffer-delete-next! b row col)
       rs)))
  (define (delete-previous)
    (do-to-buffer
     (λ (b row col)
       (if (buffer-delete-previous! b row col)
         (move-cursor -1 0)
         rs))))

  (define next-rs
    (let loop ([h empty])
      (sync
       (choice-evt
        (handle-evt ch
                    (λ (ke)
                      (match (cons ke h)
                        [(rune-key 'C-q)
                         (exit 0)]
                        [(rune-key 'C-c 'C-x)
                         (exit 0)]

                        [(rune-key 'C-<left>)
                         (move-focus -1)]
                        [(rune-key 'C-<right>)
                         (move-focus +1)]

                        [(rune-key 'C-<up>)
                         (move-meta-focus -1)]
                        [(rune-key 'C-<down>)
                         (move-meta-focus +1)]

                        [(rune-key '<left>)
                         (move-cursor -1 0)]
                        [(rune-key '<right>)
                         (move-cursor +1 0)]
                        [(rune-key '<up>)
                         (move-cursor 0 -1)]
                        [(rune-key '<down>)
                         (move-cursor 0 +1)]

                        [(rune-key (? char? c))
                         (insert-char c)]
                        [(rune-key '<return>)
                         (insert-char #\newline)]
                        [(rune-key '<space>)
                         (insert-char #\space)]
                        [(rune-key '<backspace>)
                         (delete-previous)]
                        [(rune-key '<delete>)
                         (delete-next)]

                        [x
                         (eprintf "ignored ~s\n" x)
                         (loop x)])))))))

  (loop loop ch gf next-rs))

(module+ main
  (require racket/runtime-path)
  (define-runtime-path exp "../TODO.org")
  (define ex (path->string exp))
  (start
   (rstate (hasheq 0
                   (path->buffer ex))
           (focus (ctxt:layer
                   (ctxt:top)
                   'horizontal
                   (list)
                   (list
                    (focus (ctxt:layer
                            (ctxt:top)
                            'vertical
                            (list
                             (focus (ctxt:top)
                                    (view (cursor 2 0) 0)))
                            empty)
                           (view (cursor 3 0) 0))
                    (focus (ctxt:layer
                            (ctxt:top)
                            'horizontal
                            (list)
                            (list
                             (focus (ctxt:layer
                                     (ctxt:top)
                                     'vertical
                                     (list
                                      (focus (ctxt:top)
                                             (view (cursor 2 0) 0)))
                                     empty)
                                    (view (cursor 3 0) 0))
                             (focus (ctxt:top)
                                    (view (cursor 1 0) 0))))
                           (view (cursor 0 0) 0))))
                  (view (cursor 0 0) 0)))))
