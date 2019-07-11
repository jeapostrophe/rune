#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     syntax/parse/lib/function-header)
         racket/match
         racket/generic
         racket/stxparam
         racket/sandbox
         racket/set
         lux
         struct-define
         raart)

;; XXX Think about syndicate?

;; XXX contracts
(provide
 bindings-eval
 bindings
 bindings-merge

 rune?
 rune-evt
 rune-out
 rune-del
 rune-acts
 rune-bindings

 act-ref
 @ @?

 new act screen-rows screen-cols
 define-rune

 rune-main
 rune-run

 codex?)

(begin-for-syntax
  (define (make-read-only-rename-transformer target)
    (make-set!-transformer
     (λ (stx)
       (syntax-parse stx
         #:literals (set!)
         [(set! x v)
          (raise-syntax-error (syntax-e target) "may not be set" stx #'x)]
         [x:id target])))))

;; Bindings
;; #f --- Quit
;; #t --- Pass key to key
;; se --- evaluate se and call action

(define (bindings-eval binds r e)
  (match (hash-ref binds e #t)
    [#f #f]
    [#t
     (@? r key e)
     #t]
    [(or (and (? symbol? action)
              (app (λ (_) '()) args))
         (cons (? symbol? action) args))
     (define f (act-ref r action))
     (when f (apply f (map rune-eval args)))
     #t]))

(begin-for-syntax
  (define-splicing-syntax-class bindings-stx
    (pattern (~seq [k:string ka:expr] ...))))
(define-syntax (bindings stx)
  (syntax-parse stx
    [(_ ;; xxx context or something?
      [k:string ka:expr] ...)
     (syntax/loc stx
       (make-immutable-hash
        (list (cons k 'ka)
              ...)))]))
(define (bindings-merge top bot)
  (for/fold ([bs bot])
            ([(k v) (in-hash top)])
    (hash-set bs k v)))

(define user-bindings-path
  (build-path (find-system-path 'home-dir) ".rune.rkt"))
(define (load-user-bindings)
  (or
   (and (file-exists? user-bindings-path)
        (dynamic-require user-bindings-path 'bindings #f))
   (hasheq
    "C-D" #f)))

(define rune-eval
  (make-evaluator 'racket/base))

(define should-eval-set
  (seteq #\( #\' #\"))
;; Xxx automatically parse numbers?
(define (rune-arg-eval s)
  (if (and (not (zero? (string-length s)))
           (set-member? should-eval-set (string-ref s 0)))
    (rune-eval s)
    s))

(define-generics rune
  (rune-evt rune)
  (rune-out rune *screen-rows *screen-cols)
  (rune-del rune)
  (rune-acts rune)
  (rune-bindings rune))

(define (act-ref r m-id)
  (hash-ref (rune-acts r) m-id #f))
(define-syntax-rule (@ r m)
  (act-ref r 'm))
(define-syntax-rule (@? r m . args)
  (let ([f (@ r m)])
    (and f (f . args))))

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
        (~seq #:bindings ~! bs:bindings-stx))
     #:with r (datum->syntax #'the-rune 'this)
     #:with rune-action-table (datum->syntax #'the-rune 'rune-action-table)
     (syntax/loc stx
       (begin
         (define default-bindings
           (bindings . bs))
         (struct rep (rune-action-table rune-f ...)
           #:mutable
           #:reflection-name 'the-rune
           #:methods gen:rune
           [(define (rune-evt r)
              (rep-define r)
              evt-body ...)
            (define (rune-out r *screen-rows *screen-cols)
              (syntax-parameterize
                  ([screen-rows (make-read-only-rename-transformer #'*screen-rows)]
                   [screen-cols (make-read-only-rename-transformer #'*screen-cols)])
                (rep-define r)
                out-body ...))
            (define (rune-del r)
              (rep-define r)
              del-body ...
              (void))
            (define (rune-acts r)
              (rep-define r)
              rune-action-table)
            (define (rune-bindings r)
              default-bindings)])
         (define-struct-define rep-define rep)
         (define the-rune
           (λ new-args
             new-body ...
             (define meth
               (λ meth-args
                 meth-body ...))
             ...
             (define r
               (rep (make-immutable-hasheq
                     (list (cons 'meth meth) ...))
                    f-init ...))
             (rep-define r)
             init-body ...
             r))))]))

(define-syntax (rune-main stx)
  (syntax-parse stx
    [(_ the-rune:id)
     (syntax/loc stx
       (begin
         (module+ main
           (rune-main* (symbol->string 'the-rune) the-rune))
         (module+ rune
           (provide (rename-out [the-rune rune])))))]))

(define (rune-run rune-obj)
  (define the-binds
    (bindings-merge
     ;; XXX maybe pass 'program
     (load-user-bindings)
     (rune-bindings rune-obj)))
  (call-with-chaos
   (make-raart #:mouse? #t)
   (λ () (fiat-lux (codex the-binds rune-obj 24 80))))
  (void))

(define (rune-main* program make-rune)
  (local-require racket/cmdline)
  (command-line
   #:program program
   #:args args
   (let ([rune-obj
          (apply make-rune (map rune-arg-eval args))])
     (rune-run rune-obj))))

(define-struct-define codex-define codex)
(struct codex (binds r rows cols)
  #:mutable
  #:methods gen:word
  [(define (word-fps w) 0.0)
   (define (word-label w ft)
     (codex-define w)
     (or (@? r label)
         "Rune"))
   (define (word-evt w)
     (codex-define w)
     (rune-evt r))
   (define (word-event w e)
     (codex-define w)
     (match e
       [(screen-size-report new-rows new-cols)
        (set! rows new-rows)
        (set! cols new-cols)
        (@? r screen-resized rows cols)
        w]
       [(? any-mouse-event?)
        (@? r mouse e)
        w]
       [(? string?)
        (and (bindings-eval binds r e)
             w)]
       [_
        (@? r unbound e)
        w]))
   (define (word-output w)
     (codex-define w)
     (rune-out r rows cols))
   (define (word-return w)
     (codex-define w)
     (rune-del r)
     (void))])
