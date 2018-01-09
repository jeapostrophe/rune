#lang racket/base
;; Library
(require (for-syntax racket/base
                     syntax/parse
                     syntax/parse/lib/function-header)
         racket/generic
         racket/stxparam
         lux
         struct-define
         raart)

(begin-for-syntax
  (define (make-read-only-rename-transformer target)
    (make-set!-transformer
     (λ (stx)
       (syntax-parse stx
         #:literals (set!)
         [(set! x v)
          (raise-syntax-error (syntax-e target) "may not be set" stx #'x)]
         [x:id target])))))

(define-generics rune
  (rune-evt rune)
  (rune-out rune *screen-cols *screen-rows)
  (rune-del rune)
  (rune-acts rune))

(define-syntax new
  (λ (stx) (raise-syntax-error 'new "Illegal outside of define-rune" stx)))
(define-syntax act
  (λ (stx) (raise-syntax-error 'act "Illegal outside of define-rune" stx)))
(define-syntax-parameter screen-rows
  (λ (stx) (raise-syntax-error 'new "Illegal outside of out" stx)))
(define-syntax-parameter screen-cols
  (λ (stx) (raise-syntax-error 'new "Illegal outside of out" stx)))
(define-syntax (define-rune stx)
  (syntax-parse stx
    #:literals (new act)
    [(_ the-rune:id
        (~seq #:new ~!
              (~seq new-args:formals
                    new-body:expr ...
                    (new [rune-f:id f-init:expr] ...))
              (~fail #:when (check-duplicate-identifier (syntax->list #'(rune-f ...)))
                     "fields must be unique"))
        (~optional
         (~seq #:init ~!
               (~seq init-body:expr ...))
         #:defaults ([(init-body 1) '()]))
        (~optional
         (~seq #:del ~!
               (~seq del-body:expr ...))
         #:defaults ([(del-body 1) '()]))
        (~optional
         (~seq #:evt ~!
               (~seq evt-body:expr ...+))
         #:defaults ([(evt-body 1) (list #'never-evt)]))
        (~seq #:out ~!
              (~seq out-body:expr ...+))
        (~seq #:act ~!
              (act (meth:id . meth-args:formals)
                meth-body:expr ...+)
              ...
              (~fail #:when (check-duplicate-identifier (syntax->list #'(meth ...)))
                     "actions must be unique"))
        (~seq #:bindings ~!
              ;; xxx context
              [k:string ka:expr] ...))
     #:with r (datum->syntax #'the-rune 'this)
     (syntax/loc stx
       (begin
         ;; XXX bindings
         (struct rep (rune-f ...)
           #:mutable
           #:reflection-name 'the-rune
           #:methods gen:rune
           [(define (rune-evt r)
              (rep-define r)
              evt-body ...)
            (define (rune-out r *screen-cols *screen-rows)
              (syntax-parameterize
                  ([screen-cols (make-read-only-rename-transformer #'*screen-cols)]
                   [screen-rows (make-read-only-rename-transformer #'*screen-rows)])
                (rep-define r)
                out-body ...))
            (define (rune-del r)
              (rep-define r)
              del-body ...
              (void))
            (define (rune-acts r)
              (rep-define r)
              (define meth
                (λ meth-args
                  meth-body ...))
              ...
              (make-immutable-hasheq
               (list (cons 'meth meth) ...)))])
         (define-struct-define rep-define rep)
         (define (init r)
           (rep-define r)
           init-body ...
           (void))
         (define the-rune
           (λ new-args
             new-body ...
             (define r (rep f-init ...))
             (init r)
             r))
         (rune-main the-rune)))]))

(define-syntax (rune-main stx)
  (syntax-parse stx
    [(_ the-rune:id)
     (syntax/loc stx
       (begin
         (module+ main
           (rune-main* (symbol->string 'the-rune) the-rune))
         (module+ rune
           (provide (rename-out [the-rune rune])))))]))
(define (rune-main* program make-rune)
  (local-require racket/cmdline)
  (command-line
   ;; XXX get program via an action
   #:program program
   ;; XXX help
   #:args args
   (let ([rune-obj
          ;; XXX eval args
          (apply make-rune args)])
     ;; XXX
     (exit 1)
     (call-with-chaos
      (make-raart #:mouse? #t)
      (λ () (fiat-lux (codex rune-obj))))
     (void))))

;; XXX init del evt out act bindings
(struct codex (rune)
  #:methods gen:word
  [(define (word-fps w) 0.0)
   (define (word-label w ft)
     ;; XXX or call an action
     "Rune")
   (define (word-evt w)
     ;; XXX look at evt
     never-evt)
   (define (word-event w e)
     ;; XXX decode
     (match e
       [(== (key #\D (set 'control)))
        #f]
       [_
        w]))
   (define (word-output w)
     ;; XXX
     (blank))
   (define (word-return w)
     ;; XXX
     w)])

;; Usage
(require racket/match
         racket/set
         racket/port
         racket/format
         data/gvector)

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
  ;; XXX should include label, mode, screen
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

#;(module+ main
    (require racket/cmdline)
    (define to-view
      (command-line
       #:program "rage"
       #:usage-help "racket pager"
       #:args (file)
       file))
    (void
     (with-input-source to-view page)))
