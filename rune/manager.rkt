#lang racket/base
(require rune/events
         racket/match
         racket/async-channel
         rune/colors)

(struct manager (rs cs cc evt))

(define (start-manager)
  (define to-viewer-ch (make-async-channel))
  (manager 0 0 0 to-viewer-ch))

(define (manager-resize man nrows ncols)
  (struct-copy manager man
               [rs nrows]
               [cs ncols]))

(define (screen-write! ch row col fg bg c)
  (async-channel-put ch (comm:screen-write! row col (cell fg bg c))))

(define (manager-key-event man ke)
  (match-define (manager rs cs cc ch) man)
  (cond
    [(char? ke)
     (screen-write! ch (sub1 rs) cc FG-MI BG ke)
     (struct-copy manager man
                  [cc (modulo (add1 cc) cs)])]
    [else
     man]))

(provide start-manager
         manager-resize
         manager-key-event
         manager-evt
         manager?)
