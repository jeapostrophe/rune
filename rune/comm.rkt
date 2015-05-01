#lang racket/base
(require racket/match
         racket/fasl
         racket/async-channel
         racket/tcp)

;; NOTE For now, this is programmed to use TCP. Ideally, we'd use
;; Unix-Domain-Sockets or Places

(define rune-base-port 13370)

(define ports
  (hash 'viewer->manager
        (cons #"RUNE_VIEWER_MANAGER" (+ rune-base-port 0))
        'manager->viewer
        (cons #"RUNE_MANAGER_VIEWER" (+ rune-base-port 1))
        'app->manager
        (cons #"RUNE_APP_MANAGER" (+ rune-base-port 2))))

(define (service-port service)
  (define env (current-environment-variables))
  (match-define (cons env-var default-port) (hash-ref ports service))
  (or (environment-variables-ref env env-var) default-port))

(define-syntax-rule (forever body ...)
  (let loop ()
    (let () body ...)
    (loop)))
(define (comm-listener service)
  (define msg-ch (make-async-channel))
  (define server-t
    (thread
     (λ ()
       (define p (service-port service))
       (define listener (tcp-listen p 4 #t "127.0.0.1"))
       (forever
        (with-handlers ([exn:fail? void])
          (define-values (from to) (tcp-accept listener))
          (async-channel-put msg-ch (comm:connected service))
          (forever
           (define v (fasl->s-exp from))
           (if (eof-object? v)
               (error 'done)
               (async-channel-put msg-ch v))))))))
  msg-ch)
(define (comm-sender service)
  (define msg-ch (make-async-channel))
  (define sender-t
    (thread
     (λ ()
       (define p (service-port service))
       (forever
        (with-handlers ([exn:fail? void])
          (define-values (from to) (tcp-connect "localhost" p))
          (forever
           (define v (async-channel-get msg-ch))
           (s-exp->fasl v to)
           (flush-output to)))
        ;; Wait a little bit before trying to connect again
        (sleep 1)))))
  (λ (v)
    (async-channel-put msg-ch v)))

(provide comm-listener
         comm-sender)

(module messages racket/base
  (struct cell (fg bg char) #:prefab)
  (struct comm:connected (service) #:prefab)
  (struct comm:viewer>:size (rows cols) #:prefab)
  (struct comm:viewer>:key (k) #:prefab)
  (struct comm:viewer>:ready! () #:prefab)
  (struct comm:>viewer:write! (row col c) #:prefab)
  (provide (all-defined-out)))
(require (submod "." messages))
(provide (all-from-out (submod "." messages)))
