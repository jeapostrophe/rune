#lang racket/base
(require racket/file
         racket/list
         racket/match
         racket/async-channel)

(module rune/lib/gap-buffer racket/base
  (require racket/match
           racket/list)
  (struct gap-buffer (buf buf-size gap-start gap-end) #:mutable)

  (define gap-size 256)

  (define (string->gap-buffer str)
    (define len (string-length str))
    (define new-len (+ len gap-size))
    (define new-str (make-string new-len))
    ;; The gap starts at the beginning, so we start the copy at gap-size
    (string-copy! new-str gap-size str)
    (gap-buffer new-str new-len 0 gap-size))

  (define-syntax-rule (gap-buffer-gap-start++ gb)
    (set-gap-buffer-gap-start! gb (add1 (gap-buffer-gap-start gb))))
  (define-syntax-rule (gap-buffer-gap-end++ gb)
    (set-gap-buffer-gap-end! gb (add1 (gap-buffer-gap-end gb))))
  (define-syntax-rule (gap-buffer-gap-start-- gb)
    (set-gap-buffer-gap-start! gb (sub1 (gap-buffer-gap-start gb))))
  (define-syntax-rule (gap-buffer-gap-end-- gb)
    (set-gap-buffer-gap-end! gb (sub1 (gap-buffer-gap-end gb))))

  (define (gap-buffer-forward! gb)
    (match-define (gap-buffer buf buf-size gs ge) gb)
    ;; Copy what is at ge to gs
    (define c (string-ref buf ge))
    (eprintf "forward: Copied ~a from ~a to ~a\n" c ge gs)
    (string-set! buf gs c)
    (gap-buffer-gap-start++ gb)
    (gap-buffer-gap-end++ gb))

  (define (gap-buffer-backward! gb)
    (match-define (gap-buffer buf buf-size gs ge) gb)
    ;; Copy what is at gs to ge
    (define c (string-ref buf (sub1 gs)))
    (eprintf "backward: Copied ~a from ~a to ~a\n" c (sub1 gs) ge)
    (string-set! buf ge c)
    (gap-buffer-gap-start-- gb)
    (gap-buffer-gap-end-- gb))

  (define (gap-buffer-move! gb d)
    (if (positive? d)
      (for ([i (in-range d)])
        (gap-buffer-forward! gb))
      (for ([i (in-range (- d))])
        (gap-buffer-backward! gb))))

  (define (gap-buffer-move-to! gb loc)
    (match-define (gap-buffer buf buf-size gs ge) gb)
    ;; works before below first line
    (define d 
      (if (<= loc gs)
        (- loc gs)        
        (- (add1 loc) ge)))
    (eprintf "~a move to ~a/~a\n"
             (list gs ge (- ge gs))
             loc d)
    (gap-buffer-move! gb d)
    (eprintf "-> ~a\n"             
             (list (gap-buffer-gap-start gb) (gap-buffer-gap-end gb)
                   (- (gap-buffer-gap-end gb) (gap-buffer-gap-start gb)))))

  (define (gap-buffer-insert! gb c)
    (match-define (gap-buffer buf buf-size gs ge) gb)
    ;; If they are equal, then the gap is empty
    (when (= gs ge)
      (gap-buffer-expand! gb))
    (define new-gs (gap-buffer-gap-start gb))
    (string-set! buf new-gs c)
    (eprintf "insert: Wrote ~a to ~a\n" c new-gs)
    (gap-buffer-gap-start++ gb))

  (define (gap-buffer-insert-string! gb s)
    (for ([c (in-string s)])
      (gap-buffer-insert! gb c)))

  (define (gap-buffer-expand! gb)
    (match-define (gap-buffer old-buf old-buf-size old-gs old-ge) gb)
    (define new-size (+ old-buf-size gap-size))
    (define new-buf (make-string new-size))
    (string-copy! new-buf gap-size old-buf)
    (set-gap-buffer-buf! gb new-buf)
    (set-gap-buffer-buf-size! gb new-size)
    (set-gap-buffer-gap-start! gb 0)
    (set-gap-buffer-gap-end! gb gap-size))

  ;; xxx optimize?
  (define (regexp-match-count rx input start-pos [end-pos #f])
    (length (regexp-match-positions* rx input start-pos end-pos)))

  (define newline-rx #rx"\n")
  (define (gap-buffer-newline-offsets gb)
    (match-define (gap-buffer buf size pre post) gb)
    (define before (regexp-match-positions* newline-rx buf 0 pre))
    (define after (regexp-match-positions* newline-rx buf post))
    (if (and (empty? before) (empty? after))
      ;; xxx fix
      (error 'gap-buffer-newline-offsets "No newline!")
      (map car (append before after))))

  (define (gap-buffer-line-count gb)
    (length (gap-buffer-newline-offsets gb)))

  (define (gap-buffer-line-ranges gb)
    (match-define (gap-buffer buf size pre post) gb)
    (define last 0)
    (define past-gap? #f)
    (for/list ([actual-end (gap-buffer-newline-offsets gb)])
      (define end actual-end)
      (unless past-gap?
        (when (<= post end)
          (set! end (list pre post end))
          (set! past-gap? #t)))
      (begin0 (cons last end)
              (set! last actual-end))))

  (define (gap-buffer-line-cols gb line)
    (match (list-ref (gap-buffer-line-ranges gb) line)
      [(list start mid-start mid-end end)
       (sub1 (+ (- mid-start start) (- end mid-end)))]
      [(cons start end)
       (sub1 (- end start))]))

  (define (gap-buffer-lines gb)
    (match-define (gap-buffer buf _ _ _) gb)
    (for/list ([range (in-list (gap-buffer-line-ranges gb))])
      (match range
        [(list start mid-start mid-end end)
         (string-append (substring buf start mid-start)
                        (substring buf mid-end end))]
        [(cons start end)
         (substring buf start end)])))

  (define (gap-buffer-move-to/rc! gb row col)
    (printf "moving to ~a,~a\n" row col)
    (match (list-ref (gap-buffer-line-ranges gb) row)
      [(list start mid-start mid-end end)
       (define before-mid (- mid-start start))
       (printf "line split across gap: ~e\n" 
               (list start mid-start mid-end end before-mid))
       (cond
         [(< col before-mid)          
          (printf "before mid\n")
          (gap-buffer-move-to! gb (+ start col 1))]
         [else
          (printf "after mid\n")
          (gap-buffer-move-to! gb (+ mid-start (- col before-mid)))])]
      [(cons start end)
       (gap-buffer-move-to! gb (+ start col))]))

  (provide (all-defined-out)))

(require (submod "." rune/lib/gap-buffer))

(struct buffer ())
(struct buffer:file (path content))

(define (path->buffer p)
  (buffer:file p (string->gap-buffer (file->string p))))
(define (buffer-max-row b)
  (gap-buffer-line-count (buffer:file-content b)))
(define (buffer-max-col b r)
  (gap-buffer-line-cols (buffer:file-content b) r))
(define (buffer-lines b)
  (gap-buffer-lines (buffer:file-content b)))
(define (buffer-insert-at! b row col c)
  (define gb (buffer:file-content b))
  (gap-buffer-move-to/rc! gb row col)
  (gap-buffer-insert! gb c))

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

(module rune/gui/racket racket/base
  (require racket/gui/base
           racket/async-channel
           racket/match
           racket/class)

  (struct gframe (f c pb))

  (define (key-event->rune-key ke)
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
        (define ek (key-event->rune-key event))
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
          (hash-ref! buffer->bm bid
                     (λ ()
                       (local-require (only-in racket/gui/base make-screen-bitmap))

                       (define max-row (buffer-max-row b))
                       (define max-col
                         (for/fold ([mc 0])
                             ([r (in-range max-row)])
                           (max mc (buffer-max-col b r))))

                       (define bm
                         (make-screen-bitmap
                          (inexact->exact (ceiling (* max-col char-width)))
                          (inexact->exact (ceiling (* max-row char-height)))))
                       (define bm-dc
                         (send bm make-dc))

                       (send bm-dc set-font the-font)

                       (for ([l (in-list (buffer-lines b))]
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

  (define next-rs
    (let loop ([h empty])
      (gui-sync
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
                         (match-define (focus ctxt v) (rstate-focus rs))
                         (match-define (view (cursor row col) bid) v)
                         (define b (rstate-buffer rs bid))
                         (buffer-insert-at! b row col c)
                         (hash-remove! buffer->bm bid)
                         (move-cursor +1 0)]

                        [x
                         (eprintf "ignored ~s\n" x)
                         (loop x)])))))))

  (loop loop ch gf next-rs))

(module+ main
  (start
   (rstate (hasheq 0
                   (path->buffer "../TODO.org"))
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
