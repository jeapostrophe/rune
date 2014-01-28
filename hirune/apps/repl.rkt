#lang racket/base
(require racket/sandbox
         racket/list
         racket/match
         racket/string
         hirune)

(struct world (evaler histories))
(struct history (cmd stdout stderr))

(define (initial-world)
  (world (parameterize ([sandbox-output 'string]
                        [sandbox-error-output 'string])
           (make-evaluator 'racket/base))
         empty))

(define (snoc l x)
  (append l (list x)))

(define (repl-command w mbs)
  (match-define (world evaler hs) w)
  (define maybe-error
    (with-handlers ([exn:fail?
                     (λ (x)
                       (string-append (exn-message x) "\n"))])
      (evaler mbs)
      ""))
  (define hsp
    (snoc
     hs
     (history mbs
              (string-split
               (get-output evaler)
               "\n")
              (string-split
               (string-append (get-error-output evaler)
                              maybe-error)
               "\n"))))
  (world evaler hsp))

(define (repl-render w)
  (match-define (world _ hs) w)
  (hirune-file
   #:anchor (format "#line~a" (length hs))
   `(div
     ,@(for/list ([h (in-list hs)]
                  [i (in-naturals)])
         (match-define (history c o e) h)
         `(div (span ([id ,(format "line~a" i)] [class "line"])
                     "> " ,c)
               ,@(for/list ([o (in-list o)])
                   `(span ([class "line cyan_fg"]) ,o))
               ,@(for/list ([e (in-list e)])
                   `(span ([class "line red_fg"]) ,e)))))))

(define (repl-label w)
  "REPL")

(module+ main
  (hirune #:opaque-state (initial-world)
          #:label repl-label
          #:command repl-command
          #:render repl-render))
