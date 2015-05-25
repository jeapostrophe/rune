#lang racket/base
(require net/rfc6455
         racket/runtime-path
         web-server/servlet-dispatch
         web-server/http
         web-server/dispatchers/filesystem-map
         (prefix-in seq: web-server/dispatchers/dispatch-sequencer)
         (prefix-in lift: web-server/dispatchers/dispatch-lift)
         (prefix-in files: web-server/dispatchers/dispatch-files))

(module+ main
  (define HTTP-PORT 7332)
  (define WS-PORT (+ HTTP-PORT 1))
  (define-runtime-path here ".")

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
             (let loop ()
               (define msg (ws-recv c))
               (printf "Got: ~v\n" msg)
               (ws-send! c (format "console.log(~v)" msg))
               (loop)))]])))))

  (thread-wait http-t)
  (thread-wait ws-t))

;; // http://philipwalton.github.io/solved-by-flexbox/
