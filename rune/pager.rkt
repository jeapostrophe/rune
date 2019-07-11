#lang racket/base
(require racket/match
         racket/set
         racket/port
         racket/format
         data/gvector
         rune
         raart)

(define (clamp m x M)
  (inexact->exact (min (max m x) M)))

;; XXX abstract to file-pager and obj-pager
(define-rune file-pager
  #:new (the-src)
  (new
   [src the-src] [ip (open-input-file the-src)]
   [lines (make-gvector)] [max-col 0]
   [row 0] [col 0])
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
          (gvector-add! lines (text l))
          (set! max-col (max max-col (string-length l)))]))))
  #:out
  ;; xxx vertical centering mode
  (define max-lines (gvector-count lines))
  (define drawn-lines (min screen-rows max-lines))
  (set! col (clamp 0 col (max 0 (- max-col screen-cols))))
  (set! row (clamp 0 row (max 0 (- max-lines screen-rows))))
  (crop col screen-cols 0 screen-rows
        (place-cursor-after
         (if (zero? max-lines)
           (blank)
           (vappend* #:halign 'left
                     (for/list ([dr (in-range drawn-lines)])
                       (gvector-ref lines (+ row dr)))))
         0 col))
  ;; XXX some way to expose other internal runes (and maybe register
  ;; them externally?) ... maybe communicate with parent? ... maybe
  ;; have an `internal` action that is called by evt, etc?
  #:act
  ;; XXX add documentation to actions
  (act (move-cursor dr dc)
    (set! row (clamp 0 (+ row dr) (gvector-count lines)))
    (set! col (clamp 0 (+ col dc) max-col)))
  (act    (up) (move-cursor -1  0))
  (act  (down) (move-cursor +1  0))
  (act  (left) (move-cursor  0 -1))
  (act (right) (move-cursor  0 +1))
  (act (label) (format "pager: ~a" src))
  #:bindings
  ["q" #f]
  ["<up>" up]
  ["<down>" down]
  ["<left>" left]
  ["<right>" right]
  ;; XXX kind of ugly to know about this function
  ["<home>" (move-cursor -inf.0 0)]
  ["<end>" (move-cursor +inf.0 0)]
  ;; xxx like to use or
  ["<page-down>" (move-cursor +24 0)]
  [" " (move-cursor +24 0)]
  ["<page-up>" (move-cursor -24 0)])

;; xxx argument for body's modes
;; xxx expose body's action or catch those not seen?
;; xxx having a hard time figuring out what the different between a rune and a word is
(define-rune modal
  #:new (b)
  (new [body b] [last-e #f])
  #:del
  (rune-del body)
  #:evt
  (rune-evt body)
  #:out
  (define body-rows (- screen-rows 2))
  (vappend
   #:reverse? #t
   (crop 0 screen-cols
         0 body-rows
         (rune-out body body-rows screen-cols))
   (vappend #:halign 'left
            (style 'inverse
              (text (~a "XXX Status: " last-e
                        #:min-width screen-cols)))
            (text "XXX Command")))
  #:act
  (act (key e) (set! last-e e))
  (act (label) ((@ body label)))
  #:bindings
  ["q" #f])

(module+ main
  (require racket/cmdline)
  (command-line #:program "file-pager"
                #:args (file)
                (rune-run (file-pager file))
                #;
                (rune-run (modal (file-pager file)))))

;; xxx make modal/vimlike with the ability to switch modes of
;; keys/actions and has command bar

;; xxx make terminal

;; xxx make window manager

;; xxx make detachable server

;; xxx change raart to catch and pass hangups (maybe?)
