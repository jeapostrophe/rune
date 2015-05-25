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
      (error 'buffer-row "list-ref* ~e" (list r rowi label i (length l) pre post l)))
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

(define (buffer->strings b)
  (define how-many-rows (buffer-rows b))
  (for/list ([r (in-range how-many-rows)])
    (buffer-row b r)))

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
  [buffer->strings
   (-> buffer?
       (listof string?))]
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

(module+ test
  (require rackunit)
  (define N 10)
  (define M 5)

  (define (random-char)
    (integer->char (+ (char->integer #\A) (random 26))))
  (define (random-string [len M])
    (build-string len (λ (i) (random-char))))
  (define (random-string-list)
    (build-list (random M) (λ (i) (random-string))))

  (define (test-sl sl [cmds empty])
    (test-case
     (format "~v" `(test-sl ,sl ,cmds))
     (test-s (string-append* (add-between sl "\n")) cmds)))
  (define (test-s s [cmds empty])
    (define label (format "~v" `(test-s ,s ,cmds)))
    (test-case
     label
     (define sl (string-split s #rx"\n" #:trim? #f))
     (define b (string->buffer s))
     (check-pred buffer? b "returned buffer")
     (define rows (if (empty? sl) 1 (length sl)))

     (define (check-integrity extra-label b)
       (test-case
        (format "~a ~a" label extra-label)
        (check-equal? (buffer->string b) s "preserves string")
        (check-equal? (buffer-rows b) rows "correct row count")
        (define (len-before-row row)
          (for/sum ([rs (in-list sl)]
                    [_ (in-range row)])
            (add1 (string-length rs))))
        (for ([rs (in-list sl)]
              [row (in-naturals)])
          (check-equal? (buffer-row-cols b row) (string-length rs)
                        (format "row ~a correct length" row))
          (check-equal? (buffer-row b row) rs
                        (format "row ~a correct content" row))
          (for ([c (in-string rs)]
                [col (in-naturals)])
            (check-equal? (buffer-row-col b row col) c
                          (format "row ~a/col ~a correct content" row col))

            (for* ([next-row (in-range row rows)]
                   [next-col (in-range 0 (string-length (list-ref sl next-row)))])
              (unless (and (= next-row row) (< next-col col))
                (define start (+ (len-before-row row) col))
                (define end (+ (len-before-row next-row) next-col))
                (check-equal? (buffer-substring b row col next-row next-col)
                              (substring s start end)
                              (format "[(~a,~a)->(~a,~a)] same as [~a,~a]"
                                      row col next-row next-col
                                      start end))))))))

     (check-integrity "first" b)

     (define (snoc x l)
       (append l (list x)))
     (define debugf void)

     (define (perform-commands b cmds)
       (match cmds
         [(list)
          (values b (list))]
         [(list* nc cmds)
          (match nc
            [(cmd:dn r c)
             (define-values (old nb) (buffer-delete-next b r c))

             (debugf "\t[~v] ~a [~v]\n" (buffer->string b) nc (buffer->string nb))

             (check-equal?
              (buffer->string (buffer-insert-char nb r c old))
              (buffer->string b)
              (format "dn ~a ~a with ~a [nb = ~v]" r c old (buffer->string nb)))

             (define-values (nnb undos) (perform-commands nb cmds))
             (values nnb (snoc (cmd:ic r c old) undos))]
            [(cmd:dp r c)
             (define-values (old nb) (buffer-delete-previous b r c))

             (debugf "\t[~v] ~a [~v]\n" (buffer->string b) nc (buffer->string nb))

             (check-equal?
              (buffer->string (buffer-insert-char nb r (sub1 c) old))
              (buffer->string b)
              (format "dn ~a ~a with ~a [nb = ~v]" r (sub1 c) old (buffer->string nb)))

             (define-values (nnb undos) (perform-commands nb cmds))
             (values nnb (snoc (cmd:ic r (sub1 c) old) undos))]
            [(cmd:ic r c v)
             (define nb (buffer-insert-char b r c v))

             (debugf "\t[~v] ~a [~v]\n" (buffer->string b) nc (buffer->string nb))

             (check-equal?
              (let-values ([(x y) (buffer-delete-next nb r c)])
                (buffer->string y))
              (buffer->string b)
              (format "ic ~a ~a with ~a [nb = ~v]" r c v (buffer->string nb)))

             (define-values (nnb undos) (perform-commands nb cmds))
             (values nnb (snoc (cmd:dn r c) undos))]
            [x
             (perform-commands b cmds)])]))

     (debugf "pre\n")
     (define-values (nb undos) (perform-commands b cmds))
     (debugf "post\n")
     (define-values (nnb redos) (perform-commands nb undos))

     (check-integrity "post" nnb)

     (void)))

  (struct cmd:dp (r c) #:prefab)
  (struct cmd:dn (r c) #:prefab)
  (struct cmd:ic (r c ch) #:prefab)
  (struct cmd:is (r c s) #:prefab)

  (test-sl '("HKYHC" "IDJIL" "FKMDY"))
  (test-s "H\nI\nF")
  (test-s "")
  (test-sl '())
  (test-s "RBHMB\nVLERT\nDUYBJ\nFPXLR"
          '(#s(cmd:dn 1 1)))
  (test-s "RBHMB\nVLERT\nDUYBJ\nFPXLR"
          '(#s(cmd:ic 1 1 #\a)))
  (test-s "RLRFZ" '(#s(cmd:dn 0 0) #s(cmd:ic 0 2 #\Y)))
  (test-s "RLRFZ" '(#s(cmd:ic 0 2 #\Y) #s(cmd:dn 0 0)))
  (test-s "XNLRG\nDMKHL" '(#s(cmd:dn 0 0) #s(cmd:dp 0 2) #s(cmd:ic 0 4 #\C)))

  (for ([i (in-range N)])
    (define sl (random-string-list))
    (define (random* x) (if (zero? x) x (random x)))
    (define (random-row) (random* (length sl)))
    (define (random-col r)
      (if (empty? sl)
          0
          (random* (string-length (list-ref sl r)))))
    (test-sl sl
             (for/list ([i (in-range M)])
               (define r (random-row))
               (define c (random-col r))
               (match (random 4)
                 [0 (cmd:dp r c)]
                 [1 (cmd:dn r c)]
                 [2 (cmd:ic r c (random-char))]
                 [3 (cmd:is r c (random-string))])))))
