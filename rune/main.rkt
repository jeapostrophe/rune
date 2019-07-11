#lang racket/base
(require racket/match)

;; Documents
(struct doc (w h))
(struct doc:empty doc ())
(define empty-doc (doc:empty 0 0))

(define (draw-doc wx hx dc d)
  (match d
    [(doc:empty wc hc) (void)]))

;; Browser
(require lux lux/chaos/gui)
(define-syntax-rule (fix id e) (letrec ([id e]) id))
(define base-browse (word #:fps 0.0 #:label "Rune"))
(define (browse-word d)
  (fix
   me
   (word base-browse
         #:event (λ (e)
                   (cond
                     [(eq? e 'close) #f]
                     [else me]))
         #:output (λ (w h dc)
                    (draw-doc w h dc d)))))
(define (browse d)
  (call-with-chaos
   (make-gui
    #:frame-style '(no-caption)
    #:mode 'draw)
   (λ ()
     (fiat-lux (browse-word d)))))

;; Application
(module+ main
  (browse
   empty-doc))
