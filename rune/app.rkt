#lang racket/base
(require racket/generic
         racket/list
         racket/serialize)

(struct desc (doc args))
(struct arg-desc (doc id def))

;; A cmd is either a symbol or a list with a symbol as the first

(struct disp-cap ())
(struct dc:char-term disp-cap (rows cols))
;; XXX pixel-term with WxH?
;; XXX does it support various disp stuff

(struct disp ())
;; XXX a string
(struct d:row (ds))
(struct d:col (ds))
(define d:horiz d:col)
(define d:vert d:row)
;; XXX image
;; XXX has-focus
;; XXX web display
;; XXX colors

(define-generics app
  ;; app -> #f or app
  (app-delegate app)
  ;; app -> (listof symbol?)
  (app-commands app)
  ;; app symbol? -> desc?
  (app-command-desc app cmd-id)
  ;; app cmd -> app
  (app-exec app cmd)
  ;; app disp-cap -> disp
  ;; xxx take a context to show different in different places
  (app-render app disp-cap)
  ;; xxx to-update evt
  ;; app -> serializable
  (app-serialize app)
  #:fallbacks
  [(define (app-delegate a) #f)
   (define (app-commands a) empty)])

(provide (all-defined-out))
