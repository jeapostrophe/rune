#lang racket/base
(require racket/match
         racket/async-channel
         racket/list
         rune/events
         rune/screen
         rune/colors)

;; A manager...
;; + Listens for manager control key events
;; + Runs a status line
;; + Runs a command line
;; + Directs messages to the active application/window
;; + Manages a tree of windows (drawing them)
;; + Manages a set of applications (listening for writes)

(struct application (name comm local->global))
(define (make-application name comm)
  (application name comm (make-hasheq)))

(struct window (global-id application local-id [title #:mutable] [layout #:mutable]))

;; xxx also have to do desktops
(struct abstract-layout (row col rows cols gid) #:transparent)

(struct manager (viewer-comm))

(define (make-mint)
  (define counter 0)
  (λ ()
    (set! counter (add1 counter))
    counter))

(struct comm (from to))
(define (comm-flip c)
  (match-define (comm from to) c)
  (comm to from))
(define (comm-send! c v)
  (async-channel-put (comm-to c) v))

(define (make-local-comm)
  (comm (make-async-channel)
        (make-async-channel)))

;; XXX Make a little protocol where you start this in your rune.rkt
;; and can send it messages to update values and then it will manage
;; the line and update things
(define (start-status)
  ;; xxx have this do stuff
  (define c< (make-local-comm))
  (define c> (comm-flip c<))
  (define status-t
    (thread
     (λ ()
       (comm-send! c> (evt:new-window 0))
       (comm-send! c> (evt:new-title 0 "*status*"))
       (let loop ()
         (define e (sync (comm-from c>)))
         (match e
           [(evt:resize _ r c)
            (comm-send! c> (evt:write! 0 (cmd:bg BG-HI (cmd:row (cmds:repeat c #\!)))))]
           [_
            (void)])
         (loop)))))
  c<)

(define (start-command)
  ;; xxx have this do stuff
  (define c< (make-local-comm))
  (define c> (comm-flip c<))
  (define status-t
    (thread
     (λ ()
       (comm-send! c> (evt:new-window 0))
       (comm-send! c> (evt:new-title 0 "*command*"))
       (let loop ([i 2])
         (define e (sync (comm-from c>)))
         (match e
           [(? evt:resize?)
            (comm-send! c> (evt:write! 0 #\$))
            (loop 2)]
           [(evt:key _ (? char? c))
            (define e (evt:write! 0 (cmd:posn 0 i c)))
            (comm-send! c> e)
            (loop (add1 i))]
           [e
            (loop i)])))))
  c<)

(define-syntax-rule (forever e ...)
  (let loop () e ... (loop)))

(define (start-manager #:keymap [km void])
  (define viewer> (make-local-comm))
  (define >viewer (comm-flip viewer>))
  (define status-app (make-application '*status* (start-status)))
  (define command-app (make-application '*command* (start-command)))
  (define the-mint (make-mint))
  (define id->app (make-hasheq))
  (hash-set! id->app (the-mint) status-app)
  (hash-set! id->app (the-mint) command-app)
  (define id->window (make-hasheq))

  (define rows 0)
  (define cols 0)
  (define layout-tree '())
  (define active-id #f)
  (define (layout! row col rows cols l)
    (match l
      ['() (void)]
      [(cons a d)
       (layout! row col rows cols a)
       (layout! row col rows cols d)]
      [(abstract-layout arow acol arows acols gid)
       (define win (hash-ref id->window gid))
       (when win
         (define -row
           (if (negative? arow)
               (+ rows arow)
               arow))
         (define -col
           (if (negative? acol)
               (+ cols acol)
               acol))
         (define -rows
           (inexact->exact (min arows rows)))
         (define -cols
           (inexact->exact (min acols cols)))
         (define the-lay
           (layout -row -col -rows -cols))
         (set-window-layout! win the-lay)
         (send-window-resize! win))]))
  (define (layout-tree:reflow!)
    (layout! 0 0 rows cols layout-tree))
  (define (layout-tree:resize! nrows ncols)
    (set! rows nrows)
    (set! cols ncols)
    (layout-tree:reflow!))
  (define (layout-tree:new-window! w)
    (define gid (window-global-id w))
    ;; xxx if there is room, give it a spot
    (define a (window-application w))
    (cond
      [(eq? a status-app)
       (set! layout-tree
             (cons (abstract-layout -2 0 1 +inf.0 gid)
                   layout-tree))
       (layout-tree:reflow!)]
      [(eq? a command-app)
       (set! layout-tree
             (cons (abstract-layout -1 0 1 +inf.0 gid)
                   layout-tree))
       (layout-tree:reflow!)
       (unless active-id
         ;; xxx make a function to change border too
         (set! active-id gid))]))

  (define (send-window-resize! w)
    (define lid (window-local-id w))
    (define a (window-application w))
    (define lay (window-layout w))
    (comm-send! (application-comm a)
                (evt:resize lid (layout-rows lay) (layout-cols lay))))

  (define (app-evt a)
    (match-define (application name comm local->global) a)

    (define-syntax-rule (with-window [the-window local-id] b ...)
      (let ()
        (define global-id (hash-ref local->global local-id #f))
        (when global-id
          (define the-window (hash-ref id->window global-id #f))
          (when the-window
            b ...))))

    (handle-evt
     (comm-from comm)
     (λ (ae)
       (match ae
         [(evt:new-window local-id)
          (define global-id (the-mint))
          (hash-set! local->global local-id global-id)
          (define the-window
            (window global-id a local-id "" #f))
          (hash-set! id->window global-id the-window)
          (layout-tree:new-window! the-window)]
         [(evt:new-title local-id title)
          (with-window [the-window local-id]
            ;; xxx if title is being displayed, update
            (set-window-title! the-window title))]
         [(evt:write! local-id c)
          (with-window [the-window local-id]
            (define lay (window-layout the-window))
            (when lay
              (comm-send! >viewer (evt:write! 0 (cmd:bounds lay c)))))]
         [x
          (eprintf "unmatched event from application(~v): ~e\n" name x)]))))

  (define manager-t
    (thread
     (λ ()
       (forever
        (sync
         (handle-evt
          (comm-from >viewer)
          (λ (ve)
            (match ve
              [(evt:resize _ nrows ncols)
               (layout-tree:resize! nrows ncols)]
              [(evt:key _ ke)
               (km ke)
               ;; xxx have to have a way to take some messages myself
               (when active-id
                 (define the-window (hash-ref id->window active-id #f))
                 (when the-window
                   (define local-id (window-local-id the-window))
                   (define the-app (window-application the-window))
                   (comm-send! (application-comm the-app) (evt:key local-id ke))))]
              [x
               (eprintf "unmatched event from viewer: ~e\n" x)])))
         (apply choice-evt (hash-map id->app (λ (k v) (app-evt v)))))))))

  (manager viewer>))

(define (manager-evt man)
  (comm-from (manager-viewer-comm man)))
(define (manager-resize! man nrows ncols)
  (comm-send! (manager-viewer-comm man) (evt:resize 0 nrows ncols)))
(define (manager-key-event! man ke)
  (comm-send! (manager-viewer-comm man) (evt:key 0 ke)))

(provide start-manager
         manager-resize!
         manager-key-event!
         manager-evt
         manager?)
