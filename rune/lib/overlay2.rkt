#lang racket/base
(require racket/match
         racket/list)

(struct rect ())
(struct rect:global rect ())
(struct rect:buffer rect (bid))
(struct rect:range rect (bid start-row start-col width height))

(define (rect:point bid row col)
  (rect:range bid row col 1 1))

(define empty-odb (hash))

(define (odb-set o rng k v)
  (match-define (rect:range 0 r c 1 1) rng)
  (hash-update o (cons r c)
               (λ (ht)
                 (hash-set ht k v))
               hasheq))
(define (odb-hash o ir)
  (match-define (rect:range 0 r c 1 1) ir)
  (hash-ref o (cons r c) hasheq))

(provide (all-defined-out))

(module+ test
  (require racket/runtime-path
           racket/string
           racket/file
           racket/list)
  (define-runtime-path me "overlay.rkt")

  (define f (map string-split (file->lines me)))
  (define bid 0)
  (define d
    (for/fold ([d (odb-set (odb-set empty-odb (rect:global) 'global "Global")
                           (rect:buffer bid) 'buffer me)])
        ([row (in-list f)]
         [rowi (in-naturals)])
      (define rd
        (odb-set d
                 (rect:range bid rowi 0
                             (string-length (string-append* (add-between row " ")))
                             1)
                 'row row))
      (define-values (nd last-word-end)
        (for/fold ([d rd] [word-start 0])
            ([word (in-list row)])
          (define w (string-length word))
          (define word-end (+ word-start w 1))
          (define nd (odb-set d (rect:range bid rowi word-start w 1) 'word word))
          (values nd word-end)))
      nd))

  (require rackunit)
  (for ([row (in-list f)]
        [rowi (in-naturals)])
    (for/fold ([word-start 0])
        ([word (in-list row)])
      (define w (string-length word))
      (define word-end (+ word-start w 1))

      (define ht (odb-hash d (rect:range bid rowi word-start w 1)))
      (check-equal? (hash-ref ht 'global #f) "Global")
      (check-equal? (hash-ref ht 'buffer #f) me)
      (check-equal? (hash-ref ht 'row #f) row)
      (check-equal? (hash-ref ht 'word #f) word)
      word-end)))
