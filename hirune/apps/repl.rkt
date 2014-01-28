#lang racket/base
(require racket/sandbox
         racket/list
         racket/match
         racket/format
         racket/string
         hirune)

(struct world (evaler histories) #:transparent)
(struct history (cmd res stdout stderr) #:transparent)

(define (initial-world)
  (world (parameterize ([sandbox-output 'string]
                        [sandbox-error-output 'string])
           (make-evaluator 'racket/base))
         empty))

(define (snoc l x)
  (append l (list x)))

(define (repl-command w mbs)
  (match-define (world evaler hs) w)
  (define-values
    (results read-errors)
    (with-handlers ([exn:fail?
                     (λ (x)
                       (values empty (string-append (exn-message x) "\n")))])
      (values
       (call-with-values
           (λ () (evaler mbs))
         (λ args
           (filter-map (λ (x) (if (void? x) #f (~a x)))
                       args)))
       "")))
  (define hsp
    (snoc
     hs
     (history mbs
              results
              (string-split
               (get-output evaler)
               "\n")
              (string-split
               (string-append (get-error-output evaler)
                              read-errors)
               "\n"))))
  (world evaler hsp))

(define (repl-render w)
  (match-define (world _ hs) w)
  (hirune-file
   #:anchor (format "#line~a" (max 0 (sub1 (length hs))))
   `(div
     ,@(for/list ([h (in-list hs)]
                  [i (in-naturals)])
         (match-define (history c rs o e) h)
         `(div (span ([id ,(format "line~a" i)] [class "line"])
                     "> " ,c)
               ,@(for/list ([o (in-list o)])
                   `(span ([class "line cyan_fg"]) ,o))
               ,@(for/list ([r (in-list rs)])
                   `(span ([class "line"]) ,r))
               ,@(for/list ([e (in-list e)])
                   `(span ([class "line red_fg"]) ,e)))))))

(define (repl-label w)
  "REPL")

(module+ main
  (hirune #:opaque-state (initial-world)
          #:label repl-label
          #:command repl-command
          #:render repl-render))
