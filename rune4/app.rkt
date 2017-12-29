#lang racket/base

(struct app (send!-p evt))

(define (app-send! m msg)
  ((app-send!-p m) msg))

(provide (all-defined-out))
