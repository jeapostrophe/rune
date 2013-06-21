#lang racket/base
(require racket/file
         racket/list
         racket/function
         racket/match
         racket/async-channel
         rune/lib/timing
         rune/lib/colors
         (prefix-in z: rune/lib/buffer)
         #;
         (combine-in
          (prefix-in g: rune/gui/raw)
          (prefix-in d: rune/draw/raw))
         (combine-in
          (prefix-in g: rune/gui/opengl)
          (prefix-in d: rune/draw/opengl)))

(struct buffer
        (canvas canvas-needs-update? overlay row->overlay row*col->overlay content)
        #:mutable)

(define (path->buffer p)
  (define b
    (buffer #f
            #t
            (make-hasheq (list (cons 'name p)))
            (make-hasheq)
            (make-hash)
            (z:string->buffer (file->string p))))
  ;; xxx hack to populate the overlays
  (define brco (buffer-row*col->overlay b))
  (for ([row (in-range (buffer-max-row b))])
    (for ([col (in-range (buffer-max-col b row))])
      (when (zero? (random 2))
        (define rco (hash-ref! brco (cons row col) make-hasheq))
        (hash-set! rco 'highlight? #t))))
  b)

(define (buffer-max-row b)
  (z:buffer-rows (buffer-content b)))
(define (buffer-max-col b r)
  (z:buffer-row-cols (buffer-content b) r))
(define (buffer-max-cols b)
  (for/fold ([mc 0])
      ([r (in-range (buffer-max-row b))])
    (max mc (buffer-max-col b r))))

(define (buffer-line b r)
  (z:buffer-row (buffer-content b) r))
(define (buffer-insert-at! b row col c)
  (define zb (buffer-content b))
  (define nzb (z:buffer-insert-char zb row col c))
  (set-buffer-content! b nzb)
  (void))
(define (buffer-delete-next! b row col)
  (define zb (buffer-content b))
  (with-handlers ([exn:fail? (λ (x) #f)])
    (define-values (_ nzb) (z:buffer-delete-next zb row col))
    (set-buffer-content! b nzb)
    #t))
(define (buffer-delete-previous! b row col)
  (define zb (buffer-content b))
  (with-handlers ([exn:fail? (λ (x) #f)])
    (define-values (_ nzb) (z:buffer-delete-previous zb row col))
    (set-buffer-content! b nzb)
    #t))

(define-syntax overlay-ref
  (syntax-rules ()
    [(_ () k dv)
     dv]
    [(_ (more ... last) k dv)
     (hash-ref last k (λ () (overlay-ref (more ...) k dv)))]))

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

(struct view (cursor bid))
(define (view-buffer rs v)
  (rstate-buffer rs (view-bid v)))

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

(struct rstate (overlay buffers focus))
(define-syntax-rule (define-rstate-lookup rstate-view rstate-views err)
  (define (rstate-view rs vid)
    (hash-ref (rstate-views rs) vid
              (λ ()
                (error 'rstate-view err vid)))))

(define-rstate-lookup rstate-buffer rstate-buffers "Unknown buffer ~e")

(define-colors
  color-scheme
  ui-lo     #x00 #x2b #x36
  ui-mi     #x07 #x36 #x42
  fg-hi     #x58 #x6e #x75
  fg-mi     #x65 #x7b #x83
  ui-hi     #x83 #x94 #x96
  fg-lo     #x93 #xa1 #xa1
  bg-hi     #xee #xe8 #xd5
  bg        #xfd #xf6 #xe3
  yellow    #xb5 #x89 #x00
  orange    #xcb #x4b #x16
  red       #xdc #x32 #x2f
  magenta   #xd3 #x36 #x82
  violet    #x6c #x71 #xc4
  blue      #x26 #x8b #xd2
  cyan      #x2a #xa1 #x98
  green     #x85 #x99 #x00)

(define (rstate-render! gf d rs)
  (define full-w (exact->inexact (g:frame-width gf)))
  (define full-h (exact->inexact (g:frame-height gf)))
  (define char-width (d:drawer-char-width d))
  (define char-height (d:drawer-char-height d))

  (define margin% 0.5)
  (define hmargin (* char-width margin%))
  (define vmargin (* char-height margin%))

  (define-values (bt _)
    (time-it
     42
     (for ([(bid b) (rstate-buffers rs)])
       (when (buffer-canvas-needs-update? b)
         (define rs-o (rstate-overlay rs))
         (define b-o (buffer-overlay b))

         (define max-row (buffer-max-row b))
         (define max-col (buffer-max-cols b))

         (define b-c
           (cond [(buffer-canvas b) => (λ (x) x)]
                 [else
                  (define c (d:canvas d c:bg))
                  (set-buffer-canvas! b c)
                  c]))

         (d:canvas-refresh!
          b-c max-row max-col
          (for/list ([row (in-range max-row)])
            (define l (buffer-line b row))
            (define row-o (hash-ref (buffer-row->overlay b) row make-hasheq))
            (for/list ([char (in-string l)]
                       [col (in-naturals)])
              (define col-o
                (hash-ref (buffer-row*col->overlay b) (cons row col) make-hasheq))
              (d:glyph row col
                       (if (overlay-ref (rs-o b-o row-o col-o) 'highlight? #f)
                         c:red
                         c:fg-mi)
                       c:bg
                       char))))

         (set-buffer-canvas-needs-update?! b #f)))))
  (g:frame-perf! gf 'buffers bt)

  (define (view->elements x y w h focused? v)
    (match-define (view (cursor row col) bid) v)

    (define cursor-bm-y (* row char-height))
    (define cursor-bm-x (* col char-width))

    (define b (rstate-buffer rs bid))
    (define b-c (buffer-canvas b))
    (define b-bm (d:canvas-bitmap b-c))
    (define b-bm-w (d:canvas-bitmap-width b-c))
    (define b-bm-h (d:canvas-bitmap-height b-c))
    (define ew (- w hmargin))
    (define eh (- h vmargin))

    (define (centered-d eh cursor-bm-y b-bm-h)
      (define centered-row-pxs (/ eh 2.0))
      (cond
        ;; If the cursor is so early, then we can't move things
        ;; backward, because we'd run off the screen.
        [(< cursor-bm-y centered-row-pxs)
         0.0]
        ;; If the cursor is so late, then we can't move things
        ;; forward
        [(< (- b-bm-h centered-row-pxs) cursor-bm-y)
         (- b-bm-h eh)]
        [else
         (- cursor-bm-y centered-row-pxs)]))

    (define dy (centered-d eh cursor-bm-y b-bm-h))
    (define dx (centered-d ew cursor-bm-x
                           ;; It is unclear what the right choice is
                           (if #t
                             b-bm-w
                             (* (add1 (buffer-max-col b row)) char-width))))

    (define cursor-x (- (+ x hmargin (* col char-width)) dx))
    (define cursor-y (- (+ y vmargin (* row char-height)) dy))

    ;; Render a buffer
    (list*
     ;; xxx vmargin doesn't seem to be enforced
     (g:bitmap (+ x hmargin) (+ y vmargin) ew eh
               ;; xxx put in b-bm structure?
               b-bm (d:canvas-real-width b-c) (d:canvas-real-height b-c) dx dy)

     ;; Outline
     (g:outline x y w h
                (if focused?
                  c:blue
                  c:ui-hi))

     ;; Cursor
     (unless (or (< (+ x w) (+ cursor-x char-width))
                 (< (+ y h) (+ cursor-y char-height)))
       (g:outline cursor-x cursor-y
                  char-width char-height
                  (if focused?
                    c:blue
                    c:ui-hi)))))

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

  (define-values (et elements)
    (time-it (focus->elements
              0.0 0.0
              full-w full-h
              #t (rstate-focus rs))))
  (g:frame-perf! gf 'elements et)
  (define b (view-buffer rs (focus-view (rstate-focus rs))))
  (define rs-o (rstate-overlay rs))
  (define b-o (buffer-overlay b))
  (define focused-label
    (overlay-ref (rs-o b-o) 'name ""))
  (g:frame-refresh! gf focused-label elements)
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
          (set-buffer-canvas-needs-update?! b #t)))

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
   [(or (rune-key '<space>) (rune-key 'S-<space>))
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
  (define gf (g:frame color-scheme c:bg ch))
  (define d (d:drawer (g:frame-context gf) color-scheme
                      ;; config: font
                      "Bitstream Vera Sans Mono" 10))
  (rstate-loop rstate-loop ch gf d rs))

;; We take loop as an argument so we can write tests that don't go
;; forever. Cute, huh?
(define (rstate-loop loop ch gf d rs)
  (rstate-render! gf d rs)

  (define next-rs
    (let loop ([h empty])
      (sync
       (choice-evt
        (handle-evt ch
                    (λ (ke)
                      (let/ec esc
                        (define kes (cons ke h))
                        (define-values
                          (kt transform)
                          (time-it
                           (with-handlers ([exn:misc:match?
                                            (λ (x)
                                              (eprintf "ignored ~s\n" kes)
                                              (esc (loop kes)))])
                             (the-key-handler kes))))
                        (g:frame-perf! gf 'key-handling kt)
                        (define-values (tt next-rs)
                          (time-it (transform rs)))
                        (g:frame-perf! gf 'transform tt)
                        next-rs)))))))

  (loop loop ch gf d next-rs))

(module+ main
  ;; config: load/save from file
  (require racket/runtime-path)
  (define-runtime-path exp "../TODO.org")
  (define ex (path->string exp))
  (start
   (rstate (hasheq)
           (hasheq 0
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
