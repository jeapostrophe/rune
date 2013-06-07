#lang racket/base
(require racket/match
         racket/list
         racket/string
         racket/contract)

;; xxx optimizations
;; -- append-reverse

(struct row-focus (pre ri cf post) #:transparent)
(struct col-focus (pre ci post) #:transparent)

(define buffer? row-focus?)
(define buffer-empty (row-focus empty 0 (col-focus empty 0 empty) empty))

(define (col-focus->string c)
  (match-define (col-focus pre _ post) c)
  (string-append (list->string (reverse pre)) (list->string post)))

(define (buffer-move-row-back b)
  (match b
    [(row-focus (list* row0 rowsN) row-n col rows-after)
     (row-focus rowsN (sub1 row-n)
                (col-focus empty 0 (string->list row0))
                (list* (col-focus->string col) rows-after))]
    [_
     (error 'buffer-move-row-back "Fail: ~e" b)]))

(define (buffer-move-row-forward b)
  (match b
    [(row-focus rows-before row-n col (list* row0 rowsN))
     (row-focus (list* (col-focus->string col) rows-before) (add1 row-n)
                (col-focus empty 0 (string->list row0))
                rowsN)]
    [_
     (error 'buffer-move-row-forward "Fail: ~e" b)]))

(define (buffer-move-row b row)
  (match-define (row-focus rows-before row-n cf
                           rows-after)
                b)
  (define how-many (- row-n row))
  (cond
    ;; Backward
    [(positive? how-many)
     (for/fold ([b b]) ([i (in-range how-many)])
       (buffer-move-row-back b))]
    ;; Forward
    [(negative? how-many)
     (for/fold ([b b]) ([i (in-range (abs how-many))])
       (buffer-move-row-forward b))]
    [else
     b]))

(define (buffer-move-col b col)
  (match-define (row-focus rows-before row-n
                           (col-focus cols-before col-n cols-after)
                           rows-after)
                b)
  (define how-many (- col-n col))
  (define (split-at* label l i)
    (unless (<= i (length l))
      (error 'buffer-move-col "~e" (list label b col l how-many i)))
    (split-at l i))
  (cond
    ;; Backward
    [(positive? how-many)
     (define-values (new-after new-before) (split-at* "back" cols-before how-many))
     (row-focus rows-before row-n
                (col-focus new-before col (append (reverse new-after) cols-after))
                rows-after)]
    ;; Forward
    [(negative? how-many)
     (define-values (new-before new-after) (split-at* "forward" cols-after (abs how-many)))
     (row-focus rows-before row-n
                (col-focus (append (reverse new-before) cols-before) col new-after)
                rows-after)]
    [else
     b]))

(define (buffer-move b row col)
  (buffer-move-col (buffer-move-row b row) col))

(define (buffer-insert-char b row col c)
  (match-define (row-focus rows-before row-n
                           (col-focus cols-before col-n cols-after)
                           rows-after)
                (buffer-move b row col))
  (if (char=? #\newline c)
    (row-focus (list* (col-focus->string (col-focus cols-before col-n empty)) rows-before) (add1 row-n)
               (col-focus empty 0 cols-after)
               rows-after)
    (row-focus rows-before row-n
               (col-focus (list* c cols-before) (add1 col-n) cols-after)
               rows-after)))

(define (buffer-insert-string b row col str)
  (define-values (nb _r _c)
    (for/fold ([b b] [row row] [col col]) ([c (in-string str)])
      (define nb (buffer-insert-char b row col c))
      (if (char=? #\newline c)
        (values nb (add1 row) 0)
        (values nb row (add1 col)))))
  nb)
(define (string->buffer str)
  (buffer-insert-string buffer-empty 0 0 str))

(define (buffer-rows b)
  (match-define (row-focus pre rowi _ post) b)
  (+ rowi 1 (length post)))

(define (buffer-row b r)
  (match-define (row-focus pre rowi rowic post) b)
  (define (list-ref* label l i)
    (unless (< i (length l))
      (error 'buffer-row "~e" (list r pre rowi post label l i)))
    (list-ref l i))
  (cond
    [(< r rowi)
     (list-ref* "pre" pre (- rowi r 1))]
    [(= r rowi)
     (col-focus->string rowic)]
    [else
     (list-ref* "post" post (- r rowi 1))]))

(define (buffer-row-cols b r)
  (string-length (buffer-row b r)))

(define (buffer-subrow b r s [e #f])
  (if e
    (substring (buffer-row b r) s e)
    (substring (buffer-row b r) s)))

(define (buffer-substring b start-row start-col end-row end-col)
  (string-append*
   (add-between
    (for/list ([irow (in-range start-row (add1 end-row))])
      (cond
        [(= irow start-row end-row)
         (buffer-subrow b irow start-col end-col)]
        [(= irow start-row)
         (buffer-subrow b irow start-col)]
        [(= irow end-row)
         (buffer-subrow b irow 0 end-col)]
        [else
         (buffer-row b irow)]))
    "\n")))

(define (buffer->string b)
  (define how-many-rows (buffer-rows b))
  (define last-row (sub1 how-many-rows))
  (buffer-substring b 0 0 last-row (buffer-row-cols b last-row)))

(define (buffer-row-col b r c)
  (string-ref (buffer-row b r) c))

(define (buffer-delete-previous b row col)
  (match (buffer-move b row col)
    [(row-focus (cons last-row rows-before) (== row)
                (col-focus (list) (and 0 (== col)) cols-after)
                rows-after)
     (values #\newline
             (row-focus rows-before (sub1 row)
                        (col-focus (reverse (string->list last-row)) (string-length last-row) cols-after)
                        rows-after))]
    [(row-focus rows-before (== row)
                (col-focus (cons deleted cols-before) (== col) cols-after)
                rows-after)
     (values deleted
             (row-focus rows-before row
                        (col-focus cols-before (sub1 col) cols-after)
                        rows-after))]
    [_
     (error 'buffer-delete-previous "Fail: ~e ~e ~e" b row col)]))

(define (buffer-delete-next b row col)
  (match (buffer-move b row col)
    [(row-focus rows-before (== row)
                (col-focus cols-before (== col) (list))
                (cons next-row rows-after))
     (values #\newline
             (row-focus rows-before row
                        (col-focus cols-before col (string->list next-row))
                        rows-after))]
    [(row-focus rows-before (== row)
                (col-focus cols-before (== col) (cons deleted cols-after))
                rows-after)
     (values deleted
             (row-focus rows-before row
                        (col-focus cols-before col cols-after)
                        rows-after))]
    [_
     (error 'buffer-delete-next "Fail: ~e ~e ~e" b row col)]))

(define pos? exact-nonnegative-integer?)
(provide
 (contract-out
  [buffer?
   (-> any/c
       boolean?)]
  [string->buffer
   (-> string?
       buffer?)]
  [buffer->string
   (-> buffer?
       string?)]
  [buffer-rows
   (-> buffer?
       pos?)]
  [buffer-row-cols
   (-> buffer? pos?
       pos?)]
  [buffer-row
   (-> buffer? pos?
       string?)]
  [buffer-row-col
   (-> buffer? pos? pos?
       char?)]
  [buffer-substring
   (-> buffer? pos? pos? pos? pos?
       string?)]
  [buffer-delete-previous
   (-> buffer? pos? pos?
       (values char? buffer?))]
  [buffer-delete-next
   (-> buffer? pos? pos?
       (values char? buffer?))]
  [buffer-insert-char
   (-> buffer? pos? pos? char?
       buffer?)]
  [buffer-insert-string
   (-> buffer? pos? pos? string?
       buffer?)]))
