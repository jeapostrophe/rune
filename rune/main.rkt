#lang racket/base
(require racket/file
         racket/list
         racket/function
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

(require (prefix-in g: rune/gui/racket))

(require racket/draw
         racket/class)

(define buffer->bm (make-hasheq))
;; xxx hack
(define buffer->os (make-hasheq))

;; xxx
(define the-font (make-font #:family 'modern))
(define-values (char-width char-height)
  (let ()
    (local-require (only-in racket/gui/base make-screen-bitmap))
    (define text-bm (make-screen-bitmap 200 200))
    (define text-dc (send text-bm make-dc))
    (send text-dc set-font the-font)
    (define-values (width height xtra-below xtra-above)
      (send text-dc get-text-extent " "))
    (values width height)))

(define (rstate-render! gf rs)
  (define cursor-outline-c (make-object color% 0 0 255 1.0))
  (define frame-outline-c cursor-outline-c)
  (define frame-outline-c/dull (make-object color% 127 127 127 0.5))
  (define cursor-fill-c/focus (make-object color% 0 0 255 0.5))
  (define cursor-fill-c/unfocus (make-object color% 0 0 255 0.0))

  (define full-w (g:frame-width gf))
  (define full-h (g:frame-height gf))

  (define margin% 0.5)
  (define hmargin (* char-width margin%))
  (define vmargin (* char-height margin%))

  (define (view->elements x y w h focused? v)
    (match-define (view c bid) v)

    ;; Render a buffer
    (list*
     (let ()
       (define b (rstate-buffer rs bid))

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

       (g:bitmap (+ x hmargin) (+ y vmargin)
                 (max 0 (- w hmargin)) (max 0 (- h vmargin))
                 b-bm 0 0))

     ;; Outline
     (g:outline x y w h
                (if focused?
                  frame-outline-c
                  frame-outline-c/dull))

     ;; Cursor
     (let ()
       ;; xxx not showing for non-focused
       (match-define (cursor row col) c)
       (define cursor-x (+ x hmargin (* col char-width)))
       (define cursor-y (+ y vmargin (* row char-height)))
       (unless (or (< w (+ cursor-x char-width))
                   (< h (+ cursor-y char-height)))
         (g:outline cursor-x cursor-y
                    char-width char-height
                    (if focused?
                      cursor-fill-c/focus
                      cursor-fill-c/unfocus))))))

  (define (ctxt->elements x y w h c)
    (match c
      [(ctxt:top)
       (values x y w h empty)]
      [(ctxt:layer p s l r)
       (define-values (mx my mw mh es) (ctxt->elements x y w h p))
       (match s
         ;; xxx merge these
         ['horizontal
          (define i (length l))
          (define ew (/ mw (+ i 1 (length r))))
          (define (xof j)
            (+ mx (* j ew)))

          (values (xof i) my ew mh
                  (list* (for/list ([sf (in-list l)]
                                    [rj (in-naturals 1)])
                           (define j (- i rj))
                           (focus->elements (xof j) my ew mh #f sf))
                         (for/list ([sf (in-list r)]
                                    [aj (in-naturals 1)])
                           (define j (+ i aj))
                           (focus->elements (xof j) my ew mh #f sf))
                         es))]
         ['vertical
          (define i (length l))
          (define eh (/ mh (+ i 1 (length r))))
          (define (yof j)
            (+ my (* j eh)))

          (values mx (yof i) mw eh
                  (list* (for/list ([sf (in-list l)]
                                    [rj (in-naturals 1)])
                           (define j (- i rj))
                           (focus->elements mx (yof j) mw eh #f sf))
                         (for/list ([sf (in-list r)]
                                    [aj (in-naturals 1)])
                           (define j (+ i aj))
                           (focus->elements mx (yof j) mw eh #f sf))
                         es))])]))
  (define (focus->elements x y w h focused? f)
    (match-define (focus c v) f)
    (define-values (mx my mw mh es) (ctxt->elements x y w h c))
    (cons (view->elements mx my mw mh focused? v) es))

  (define before-render (current-inexact-milliseconds))
  (define elements
    (focus->elements 0 0 full-w full-h #t (rstate-focus rs)))
  (define focused-label
    (buffer:file-path (rstate-buffer rs (view-buffer (focus-view (rstate-focus rs))))))
  (define after-render (current-inexact-milliseconds))
  (g:frame-refresh!
   gf
   ;; xxx doesn't include frame-refresh!
   ;; xxx doesn't include key handling code
   (format "~ams: ~a"
           (real->decimal-string (- after-render before-render))
           focused-label)
   elements)
  (void))

;; Actions
(define-match-expander rune-key
  (syntax-rules ()
    [(_ e ...) (list* e ... _)]))

(define ((move-cursor dc dr) rs)
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

(define ((move-focus df) rs)
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

(define ((move-meta-focus df) rs)
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

(define (do-to-buffer rs f)
  (match-define (focus ctxt v) (rstate-focus rs))
  (match-define (view (cursor row col) bid) v)
  (define b (rstate-buffer rs bid))
  (begin0 (f b row col)
          (hash-remove! buffer->bm bid)))

(define ((insert-char c) rs)
  (do-to-buffer rs
                (λ (b row col)
                  (buffer-insert-at! b row col c)
                  ((move-cursor +1 0) rs))))
(define ((delete-next) rs)
  (do-to-buffer rs
                (λ (b row col)
                  (buffer-delete-next! b row col)
                  rs)))
(define ((delete-previous) rs)
  (do-to-buffer rs
                (λ (b row col)
                  (if (buffer-delete-previous! b row col)
                    ((move-cursor -1 0) rs)
                    rs))))

;; Key handler
(define the-key-handler
  (match-lambda
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

   [(rune-key '<resize>)
    identity]))

;; Start
(define (start rs)
  (define ch (make-async-channel))
  (define gf (g:frame ch))
  (rstate-loop rstate-loop ch gf rs))

;; We take loop as an argument so we can write tests that don't go
;; forever. Cute, huh?
(define (rstate-loop loop ch gf rs)
  (rstate-render! gf rs)

  (define next-rs
    (let loop ([h empty])
      (sync
       (choice-evt
        (handle-evt ch
                    (λ (ke)
                      (let/ec esc
                        (define kes (cons ke h))
                        (define transform
                          (with-handlers ([exn:misc:match?
                                           (λ (x)
                                             (eprintf "ignored ~s\n" kes)
                                             (esc (loop kes)))])
                            (the-key-handler kes)))
                        (transform rs))))))))

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
