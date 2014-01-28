#lang racket/base
(require racket/async-channel)

(define (writeln v [out (current-output-port)])
  (write v out)
  (newline out)
  (flush-output out))

(define (read-evt ip)
  (define read-ch (make-async-channel))
  (define read-t
    (thread
     (λ ()
       (let loop ()
         (define v (read ip))
         (async-channel-put read-ch v)
         (unless (eof-object? v)
           (loop))))))
  read-ch)

(provide (all-defined-out))
