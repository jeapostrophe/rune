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
  (test-case
   (format "~v" `(test-s ,s ,cmds))
   (define sl (string-split s #rx"\n" #:trim? #f))
   (define b (string->buffer s))
   (check-pred buffer? b "returned buffer")
   (define rows (if (empty? sl) 1 (length sl)))

   (define (check-integrity b)
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
                                   start end)))))))

   (check-integrity b)

   (define (perform-commands b cmds)
     (match cmds
       [(list)
        (values b (list))]
       [(list* nc cmds)
        (match nc
          [(cmd:dn r c)
           (define-values (old nb) (buffer-delete-next b r c))
           (check-equal?
            (buffer->string (buffer-insert-char nb r c old))
            (buffer->string b)
            (format "dn ~a ~a with ~a" r c old))
           
           (define-values (nnb undos) (perform-commands nb cmds))
           (values nnb (cons (cmd:ic r c old) undos))]
          [x
           (perform-commands b cmds)])]))

   (define-values (nb undos) (perform-commands b cmds))
   (define-values (nnb redos) (perform-commands nb undos))

   (check-integrity nnb)

   (void)))

(struct cmd:dp (r c) #:prefab)
(struct cmd:dn (r c) #:prefab)
(struct cmd:ic (r c ch) #:prefab)
(struct cmd:is (r c s) #:prefab)

(module+ test
  (test-sl '("HKYHC" "IDJIL" "FKMDY"))
  (test-s "H\nI\nF")
  (test-s "")
  (test-sl '()))

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
