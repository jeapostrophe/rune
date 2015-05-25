#lang racket/base

(struct manager (send!-p evt))

(define (manager-send! m msg)
  ((manager-send!-p m) msg))

(provide (all-defined-out))
