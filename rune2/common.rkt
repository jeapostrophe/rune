#lang racket/base
(require racket/contract/base)

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

(provide
 (contract-out
  [struct event ()]
  [struct event:uzbl
          ([instance (or/c symbol? exact-nonnegative-integer?)]
           [name symbol?]
           [details string?])]
  [struct event:rune ()]
  [struct event:rune:key
          ([c (or/c char? symbol?)])]
  [struct event:rune:status
          ([m string?])]))
