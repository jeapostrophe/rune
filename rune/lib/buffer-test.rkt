#lang racket/base
(require rackunit
         racket/string
         racket/match
         racket/list
         "buffer.rkt")

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

(module+ test
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
  (test-s "XNLRG\nDMKHL" '(#s(cmd:dn 0 0) #s(cmd:dp 0 2) #s(cmd:ic 0 4 #\C))))

(module+ main
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
