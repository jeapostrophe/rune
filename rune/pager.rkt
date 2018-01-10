#lang racket/base
(require racket/match
         racket/set
         racket/port
         racket/format
         data/gvector
         rune
         raart)

(define (clamp m x M)
  (min (max m x) M))

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
  (crop col (add1 screen-cols) row (add1 screen-rows)
        (place-cursor-after
         (if (zero? (gvector-count lines))
           (blank)
           (vappend* #:halign 'left
                     (for/list ([e (in-gvector lines)])
                       e)))
         row col))
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
  ["<up>" (up)]
  ["<down>" (down)]
  ["<left>" (left)]
  ["<right>" right])

;; xxx make modal/vimlike with the ability to switch modes of
;; keys/actions and has command bar

;; xxx make terminal

;; xxx make window manager

;; xxx make detachable server

;; xxx change raart to catch and pass hangups
