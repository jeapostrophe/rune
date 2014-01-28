#lang racket/base
(require racket/contract/base)

(define name/c
  (or/c symbol? exact-nonnegative-integer?))
(provide (contract-out [name/c contract?]))

(define-syntax-rule (structc name (parent ...) ([field field/c] ...))
  (begin (struct name parent ... (field ...) #:prefab)
         (provide (contract-out [struct name ([field field/c] ...)]))))

(structc event () ())
(structc event:uzbl (event)
         ([instance name/c] [name symbol?] [details string?]))
(structc event:hirune (event) ())
(structc event:hirune:key (event:hirune)
         ([c (or/c char? symbol?)]))
(structc event:hirune:status (event:hirune)
         ([m string?]))
(structc event:hirune:command (event:hirune)
         ([c string?]))

(structc command () ())
(structc command:exit (command) ())
(structc command:uzbl (command) ())
(structc command:uzbl:send (command:uzbl)
         ([name name/c] [cmd string?]))
(structc command:uzbl:attach (command:uzbl)
         ([name name/c]))
(structc command:hirune (command) ())
(structc command:hirune:update (command:hirune)
         ([after string?] [pbs bytes?]))
