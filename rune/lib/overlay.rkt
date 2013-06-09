#lang racket/base
(require racket/match
         racket/list)

(struct rect ())
(struct rect:global rect ())
(struct rect:buffer rect (bid))
(struct rect:range rect (bid start-row start-col width height))
(define (rect:point bid row col)
  (rect:range bid row col 1 1))

(define (rect-inside? out in)
  (match* (out in)
    [((rect:global) _) #t]
    [(_ (rect:global)) #f]
    [((rect:buffer bid) (rect:buffer bid)) #t]
    [((rect:buffer x) (rect:buffer y)) #f]
    [((rect:buffer bid) (rect:range bid _ _ _ _)) #t]
    [((rect:buffer x) (rect:range y _ _ _ _)) #f]
    [((rect:range bid or oc ow oh) (rect:range bid ir ic iw ih))
     (and (<= or ir (+ or oh))
          (<= or (+ ir ih) (+ or oh))
          (<= oc ic (+ oc ow))
          (<= oc (+ ic iw) (+ oc ow)))]
    [((rect:range x _ _ _ _) (rect:range y _ _ _ _)) #f]))

(struct entry (r k v))

(define empty-odb empty)

(define (snoc x l)
  (append l (list x)))
(define (odb-set o r k v)
  (snoc (entry r k v) o))
(define (odb-remove o ir rk)
  (filter-not
   (match-lambda
    [(entry or k v)
     (and (rect-inside? or ir) (equal? rk k))])
   o))
(define (odb-hash o ir)
  (for/fold ([ht (hasheq)]) ([e (in-list o)])
    (match-define (entry or k v) e)
    (if (rect-inside? or ir)
      (hash-set ht k v)
      ht)))

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
