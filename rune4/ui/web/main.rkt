#lang racket/base
(require net/rfc6455
         racket/runtime-path
         racket/match
         racket/contract/base
         racket/async-channel
         json
         web-server/servlet-dispatch
         web-server/http
         web-server/dispatchers/filesystem-map
         (prefix-in seq: web-server/dispatchers/dispatch-sequencer)
         (prefix-in lift: web-server/dispatchers/dispatch-lift)
         (prefix-in files: web-server/dispatchers/dispatch-files)
         rune/app
         rune/events)

(define-runtime-path here ".")

;; XXX discover a good one
(define HTTP-PORT 7333)
(define WS-PORT (+ HTTP-PORT 1))

(define (web->rune msg)
  (match msg
    [`("open!")
     (event:rune:cmd `(open!))]
    [`("key!" ,code ,alt? ,ctrl? ,meta? ,shift?)
     (define kc
       (match code
         [(regexp #rx"^U\\+(....)$" (list _ num))
          (define c (integer->char (string->number num 16)))
          (if shift?
              (char-upcase c)
              (char-downcase c))]
         [_
          (string->symbol (string-downcase code))]))
     (event:rune:key (key-event->rune-key kc alt? ctrl? meta? shift?))]))

(define (start-rune-web m)
  (define http-t
    (thread
     (λ ()
       (serve/launch/wait
        (λ (sema)
          (seq:make
           (files:make #:url->path (make-url->path here))
           (lift:make
            (λ (req)
              (response/output
               (λ (op) (display "Not Found" op))
               #:message #"Not Found"
               #:code 404)))))
        #:port HTTP-PORT
        #:banner? #f
        #:launch-path "/"))))

  (define ws-t
    (thread
     (λ ()
       (ws-serve*
        #:port WS-PORT
        (ws-service-mapper
         ["/rune"
          [(#f)
           (lambda (c)
             (define reader-t
               (thread
                (λ ()
                  (let loop ()
                    (define wc-msg (ws-recv c))
                    (unless (eof-object? wc-msg)
                      (define msg (read-json (open-input-string wc-msg)))
                      (define e (web->rune msg))
                      (async-channel-put from-web-ch e)
                      (loop))))))

             (define writer-t
               (thread
                (λ ()
                  (let loop ()
                    (define msg (async-channel-get to-web-ch))
                    (ws-send! c msg)
                    (loop)))))

             (thread-wait reader-t)
             (kill-thread writer-t)

             (ws-close! c))]])))))

  (define from-web-ch (make-async-channel))
  (define from-web-evt
    (handle-evt from-web-ch
                (λ (e)
                  (app-send! m e))))
  (define to-web-ch (make-async-channel))
  (define to-web-evt
    (handle-evt (app-evt m)
                (λ (e)
                  (async-channel-put to-web-ch e))))

  (define manager-t
    (thread
     (λ ()
       (let loop ()
         (sync from-web-evt
               to-web-evt)
         (loop)))))

  (thread-wait manager-t)
  (thread-wait http-t)
  (thread-wait ws-t))

(provide
 (contract-out
  [start-rune-web
   (-> app?
       void?)]))