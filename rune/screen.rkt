#lang racket/base
(require racket/match)

(struct screen (rows cols row-vector))
(define default-cell #f)

(define (make-screen #:rows rows #:cols cols)
  (screen rows cols
          (build-vector rows
                        (λ (rowi)
                          (make-row #:cols cols)))))
(define (make-row #:cols cols)
  (make-vector cols default-cell))
(define (screen-write! sc row col c)
  (match-define (screen rows cols row-vector) sc)
  (when (and (< row rows) (< col cols))
    (vector-set! (vector-ref row-vector row) col c)))
(define (screen-copy! source dest)
  (for ([row (in-vector (screen-row-vector source))]
        [rowi (in-naturals)])
    (for ([c (in-vector row)]
          [coli (in-naturals)])
      (screen-write! dest rowi coli c))))

(provide (all-defined-out))
