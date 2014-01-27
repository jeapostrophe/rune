#lang racket/base
(require racket/contract/base)

(define name/c
  (or/c symbol? exact-nonnegative-integer?))

(struct event ()
        #:prefab)
(struct event:uzbl event
        (instance name details)
        #:prefab)
(struct event:rune event ()
        #:prefab)
(struct event:rune:key event:rune
        (c)
        #:prefab)
(struct event:rune:status event:rune
        (m)
        #:prefab)

(struct command ()
        #:prefab)
(struct command:exit command ()
        #:prefab)
(struct command:uzbl command ()
        #:prefab)
(struct command:uzbl:send command:uzbl 
        (name cmd)
        #:prefab)
(struct command:uzbl:attach command:uzbl 
        (name)
        #:prefab)

(provide
 (contract-out
  [name/c contract?]
  [struct event ()]
  [struct event:uzbl
          ([instance name/c]
           [name symbol?]
           [details string?])]
  [struct event:rune ()]
  [struct event:rune:key
          ([c (or/c char? symbol?)])]
  [struct event:rune:status
          ([m string?])]
  [struct command ()]
  [struct command:exit ()]
  [struct command:uzbl ()]
  [struct command:uzbl:attach 
          ([name name/c])]
  [struct command:uzbl:send 
          ([name name/c]
           [cmd string?])]))
