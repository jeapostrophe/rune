#lang racket/base

(define-syntax-rule (time-it e)
  (let* ([before (current-inexact-milliseconds)]
         [v e]
         [after (current-inexact-milliseconds)])
    (values (- after before) v)))

(provide time-it)
