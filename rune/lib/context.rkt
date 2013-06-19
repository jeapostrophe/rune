#lang racket/base
(require racket/contract)
(define context/c
  (-> (-> any) any))
(provide context/c)
