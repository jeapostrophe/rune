#lang racket/base
;; Library

;; Usage
(require racket/match
         racket/set
         racket/port
         racket/format
         data/gvector
         lux
         raart)

(define (clamp m x M)
  (min (max m x) M))

;; XXX abstract to file-pager and obj-pager
;; XXX how to find this rune in this file?
(define-rune file-pager
  #:new (the-src)
  (new
   [src the-src] [ip (open-input-file src)]
   [lines (make-gvector)] [max-col 0]
   [row 0] [col 0])
  #:init
  #:del
  (unless (or (not ip) (port-closed? ip))
    (close-input-port ip))
  #:evt
  (if (or (not ip) (port-closed? ip))
    never-evt
    (handle-evt
     (read-line-evt ip 'linefeed)
     (λ (l)
       (cond
         [(eof-object? l)
          (close-input-port ip)
          (set! ip #f)]
         [else
          (gvector-add! lines (text e))
          (set! max-col (max max-col (string-length e)))]))))
  #:out
  ;; XXX should include label, mode, screen
  (crop col (add1 screen-cols) row (add1 screen-rows)
        (place-cursor-after
         (if (zero? (gvector-count ls))
           (blank)
           (vappend* #:halign 'left
                     (for/list ([e (in-gvector ls)])
                       e)))
         row col))
  ;; XXX some way to expose other internal runes (and maybe register
  ;; them externally?) ... maybe communicate with parent? ... maybe
  ;; have an `internal` action that is called by evt, etc?
  #:act
  ;; XXX immediate / normal mode action / only sometimes enabled/allowed?
  ;; XXX default key action?
  ;; XXX screen resized
  (act (screen-resized w h) (void))
  ;; XXX add documentation to actions
  (act (move-cursor dr dc)
    (set! row (clamp 0 (+ row dr) (gvector-count lines)))
    (set! col (clamp 0 (+ col dc) max-col)))
  (act    (up) (move-cursor -1  0))
  (act  (down) (move-cursor +1  0))
  (act  (left) (move-cursor  0 -1))
  (act (right) (move-cursor  0 +1))
  #:bindings
  ["<up>" (up)]
  ["<down>" (down)]
  ["<left>" (left)]
  ["<right>" (right)])

;; xxx something to connect to args, or assume all args are strings?
;; that won't work with the obj-pager --- just parse and call eval?
(rune-main file-pager)

;;; Old version

(define (with-input-source src f)
  ((match src
     ['stdin call-with-closing-stdin]
     [_ (λ (f) (call-with-input-file src (λ (ip) (f src ip))))])
   f))
(define (call-with-closing-stdin f)
  (define cip (current-input-port))
  (dynamic-wind void (λ () (f "stdin" cip)) (λ () (close-input-port cip))))
(struct pager (src ip rows cols lines max-col row col)
  #:methods gen:word
  [(define (word-fps w) 0.0)
   (define (word-label w ft)
     (~a "rage: " (pager-src w)))
   (define (word-evt w)
     (define ip (pager-ip w))
     (if (port-closed? ip)
       never-evt
       (handle-evt (read-line-evt ip 'linefeed)
                   (λ (l)
                     (when (eof-object? l)
                       (close-input-port ip))
                     l))))
   (define (word-event w e)
     (match e
       [(== (key #\D (set 'control)))
        #f]
       [(? key?)
        (define-values (dr dc)
          (match (key-value e)
            ['up    (values -1  0)]
            ['down  (values +1  0)]
            ['left  (values  0 -1)]
            ['right (values  0 +1)]
            [_      (values  0  0)]))
        (struct-copy pager w
                     [row (clamp 0 (+ (pager-row w) dr)
                                 (gvector-count (pager-lines w)))]
                     [col (clamp 0 (+ (pager-col w) dc) (pager-max-col w))])]
       [(? string?)
        (gvector-add! (pager-lines w) (text e))
        (struct-copy pager w
                     [max-col (max (pager-max-col w) (string-length e))])]
       [(screen-size-report rows cols)
        (struct-copy pager w
                     [rows rows]
                     [cols cols])]
       [_
        w]))
   (define (word-output w)
     (define ls (pager-lines w))
     (define row (add1 (pager-row w)))
     (define col (add1 (pager-col w)))
     (crop (sub1 col) (add1 (pager-cols w)) (sub1 row) (add1 (pager-rows w))
           (place-cursor-after
            (if (zero? (gvector-count ls))
              (blank)
              (vappend* #:halign 'left
                        (for/list ([e (in-gvector ls)])
                          e)))
            (sub1 row) (sub1 col))))])

(define (page src ip)
  (call-with-chaos
   (make-raart)
   (λ ()
     (fiat-lux (pager src ip 24 80 (make-gvector) 0 0 0)))))

(module+ main
  (require racket/cmdline)
  (define to-view
    (command-line
     #:program "rage"
     #:usage-help "racket pager"
     #:args maybe-file
     (match maybe-file
       [(or (list) (list "-")) 'stdin]
       [(list one) one]
       [_ (error 'rage "Too many arguments")])))
  (void
   (with-input-source to-view page)))
