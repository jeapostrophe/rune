#lang racket/base
(require racket/match
         racket/string
         racket/list)
(module+ test
  (require rackunit)
  (define N 10)
  (define M 10)
  (define (random-char)
    (integer->char (+ (char->integer #\A) (random 26))))
  (define (random-input [len M])
    (build-string len (λ (i) (random-char)))))

(define debugf void)

(struct gap-buffer (buf buf-size gap-start gap-end) #:mutable)

(define current-gap-size (make-parameter 8))

(define (make-indexed-string len)
  (build-string len
                (λ (i) (integer->char (+ (char->integer #\0)
                                         (modulo i 10))))))

(define (string->gap-buffer str)
  (define len (string-length str))
  (define gap-size (current-gap-size))
  (define new-len (+ len gap-size))
  (define new-str (make-indexed-string new-len))
  ;; The gap starts at the beginning, so we start the copy at gap-size
  (string-copy! new-str gap-size str)
  (gap-buffer new-str new-len 0 gap-size))

(define (gap-buffer->string gb)
  (match-define (gap-buffer buf buf-size gs ge) gb)
  (string-append (substring buf 0 gs)
                 (substring buf ge)))

(define-syntax-rule (gap-buffer-gap-start++ gb)
  (set-gap-buffer-gap-start! gb (add1 (gap-buffer-gap-start gb))))
(define-syntax-rule (gap-buffer-gap-end++ gb)
  (set-gap-buffer-gap-end! gb (add1 (gap-buffer-gap-end gb))))
(define-syntax-rule (gap-buffer-gap-start-- gb)
  (set-gap-buffer-gap-start! gb (sub1 (gap-buffer-gap-start gb))))
(define-syntax-rule (gap-buffer-gap-end-- gb)
  (set-gap-buffer-gap-end! gb (sub1 (gap-buffer-gap-end gb))))

(define (gap-buffer-forward! gb)
  (match-define (gap-buffer buf buf-size gs ge) gb)
  ;; Copy what is at ge to gs
  (define c (string-ref buf ge))
  (debugf "forward: Copied ~a from ~a to ~a\n" c ge gs)
  (string-set! buf gs c)
  (gap-buffer-gap-start++ gb)
  (gap-buffer-gap-end++ gb))

(module+ test
  (define (test-forward input)
    (define gb (string->gap-buffer input))
    (for ([i (in-range (string-length input))])
      (gap-buffer-forward! gb)
      (check-equal? (gap-buffer->string gb) input)))

  (for ([i (in-range N)])
    (test-forward (random-input))))

(define (gap-buffer-backward! gb)
  (match-define (gap-buffer buf buf-size gs ge) gb)
  ;; Copy what is at gs to ge
  (define c (string-ref buf (sub1 gs)))
  (debugf "backward: Copied ~a from ~a to ~a\n" c (sub1
                                                   gs) ge)
  (string-set! buf (sub1 ge) c)
  (gap-buffer-gap-start-- gb)
  (gap-buffer-gap-end-- gb))

(module+ test
  (define (test-backward input)
    (for* ([i (in-range (string-length input))]
           [j (in-range i)])
      (define gb (string->gap-buffer input))
      ;; Go forward i
      (define last-forward
        (for/fold ([last (format "~a" (gap-buffer-buf gb))]) ([_ (in-range i)])
          (gap-buffer-forward! gb)
          (define msg (format "~a +1 ~a" last (gap-buffer-buf gb)))
          (check-equal? (gap-buffer->string gb) input msg)
          msg))
      ;; Go back j
      (for/fold ([last last-forward]) ([_ (in-range j)])
        (gap-buffer-backward! gb)
        (define msg (format "~a -1 ~a" last (gap-buffer-buf gb)))
        (check-equal? (gap-buffer->string gb) input msg)
        msg)
      (void)))

  (test-backward "XRTPA")

  (for ([i (in-range N)])
    (test-backward (random-input))))

(define (gap-buffer-move! gb d)
  (if (positive? d)
    (for ([i (in-range d)])
      (gap-buffer-forward! gb))
    (for ([i (in-range (- d))])
      (gap-buffer-backward! gb))))

(define (gap-buffer-move-to! gb loc)
  (match-define (gap-buffer buf buf-size gs ge) gb)
  ;; works before below first line
  (define d (- loc gs))
  (debugf "~a move to ~a/~a\n"
          (list gs ge (- ge gs))
          loc d)
  (gap-buffer-move! gb d)
  (debugf "-> ~a\n"
          (list (gap-buffer-gap-start gb) (gap-buffer-gap-end gb)
                (- (gap-buffer-gap-end gb) (gap-buffer-gap-start gb)))))

(module+ test
  (define (test-move-seq input moves)
    (define gb (string->gap-buffer input))
    (for/fold ([prev ""]) ([l (in-list moves)])
      (define before (string-copy (gap-buffer-buf gb)))
      (gap-buffer-move-to! gb l)
      (define after (string-copy (gap-buffer-buf gb)))
      (define msg (format "~a\nMove to ~a in ~v, [~v -> ~v]"
                          prev
                          l input
                          before after))
      (test-case msg
                 (check-equal? (gap-buffer-gap-start gb) l)
                 (check-equal? (gap-buffer->string gb) input))
      msg)
    (void))

  (test-move-seq "JJFSU" (list 4 1))

  (define (random* n)
    (if (zero? n) 0 (random n)))
  (define (random-moves [len M])
    (build-list N (λ (i) (random* len))))

  (for ([i (in-range N)])
    (test-move-seq (random-input) (random-moves))))

(define (gap-buffer-insert! gb c)
  (let ()
    (match-define (gap-buffer buf buf-size gs ge) gb)
    ;; If they are equal, then the gap is empty
    (when (= gs ge)
      (gap-buffer-expand! gb)))
  (let ()
    (match-define (gap-buffer buf buf-size gs ge) gb)
    (debugf "insert, before: ~a\n" buf)
    (string-set! buf gs c)
    (debugf "insert: Wrote ~a to ~a\n" c gs)
    (debugf "insert, after: ~a\n" buf)
    (gap-buffer-gap-start++ gb)))

(module+ test
  (define (lame-insert s where what)
    (string-append (substring s 0 where)
                   (string what)
                   (substring s where)))

  (define (test-insert-seq i inserts)
    (define gb (string->gap-buffer i))
    (for/fold ([last (format "~a" (gap-buffer-buf gb))]
               [lame i])
        ([ins (in-list inserts)])
      (match-define (cons where what) ins)
      (gap-buffer-move-to! gb where)
      (gap-buffer-insert! gb what)
      (define new (lame-insert lame where what))
      (define msg (format "~a --~a,~a-- ~a"
                          last
                          where what
                          (gap-buffer-buf gb)))
      (check-equal? (gap-buffer->string gb) new msg)
      (values msg new))
    (void))

  (parameterize ([current-gap-size 1])
    (test-insert-seq "VXDCLHSOKN"
                     '((9 . #\T)
                       (7 . #\K))))

  (parameterize ([current-gap-size 1])
    (test-insert-seq "AB"
                     (list (cons 1 #\O)
                           (cons 0 #\Q))))

  (for ([i (in-range N)])
    (test-insert-seq (random-input)
                     (build-list N (λ (i) (cons (random M) (random-char)))))))

(define (gap-buffer-insert-string! gb s)
  (for ([c (in-string s)])
    (gap-buffer-insert! gb c)))

(define (gap-buffer-expand! gb)
  (match-define (gap-buffer old-buf old-buf-size old-gs old-ge) gb)
  (define gap-size (current-gap-size))
  (define new-size (+ old-buf-size gap-size))
  (define new-buf (make-indexed-string new-size))
  (string-copy! new-buf gap-size old-buf)
  (debugf "~a -> ~a\n" old-buf new-buf)
  (set-gap-buffer-buf! gb new-buf)
  (set-gap-buffer-buf-size! gb new-size)
  (set-gap-buffer-gap-start! gb 0)
  (set-gap-buffer-gap-end! gb gap-size)
  (gap-buffer-move-to! gb old-gs))

;; xxx gap-buffer-delete!
;; xxx gap-buffer-backspace!
;; xxx gap-buffer-overwrite!
;; xxx movement by more than one instantly?

;; xxx optimize?
(define (gap-buffer-virtual->actual gb vpos)
  ...)
(define (gap-buffer-actual->virtual gb pos)
  ...)
(define (gap-buffer-newline-virtual-offsets gb)
  (define newline-rx #rx"\n")
  (match-define (gap-buffer buf size pre post) gb)
  (define before (regexp-match-positions* newline-rx buf 0 pre))
  (define after (regexp-match-positions* newline-rx buf post))

  (define normal (map car (append before after)))
  (define fake-newline-at-end
    (cond
      [(empty? normal)
       (list size)]
      [(not (empty? normal))
       (define l (last normal))
       (if (= l (sub1 size))
         empty
         (list size))]
      [else
       empty]))
  (append normal fake-newline-at-end))
(define (gap-buffer-rows gb)
  (length (gap-buffer-newline-virtual-offsets gb)))
(define (gap-buffer-row-virtual gb row)
  (list-ref (gap-buffer-newline-virtual-offsets gb) row))
(define (gap-buffer-row-cols gb row)
  (- (virtual->actual (gap-buffer-row-virtual gb (add1 row)))
     (virtual->actual (gap-buffer-row-virtual gb row))))
(define (gap-buffer-actual-substring gb astart aend)
  ...)
(define (gap-buffer-row gb row)
  (gap-buffer-actual-substring
   gb
   (gap-buffer-virtual->actual (gap-buffer-row-virtual gb (add1 row)))
   (gap-buffer-virtual->actual (gap-buffer-row-virtual gb row))))
(define (gap-buffer-move-to-rc! gb row col)
  (gap-buffer-move-to!
   gb
   (gap-buffer-actual->virtual
    (+ (gap-buffer-virtual->actual (gap-buffer-row-virtual gb row)) col))))

(module+ test
  (define (random-list-input)
    (for/list ([i (in-range (random M))]) (random-input (random M))))

  (define (test-rows-inner i moves e [li #f])
    (define gb (string->gap-buffer i))
    (test-case
     (format "test-rows ~v ~v ~v ~v" i moves e li)
     (check-equal? (gap-buffer-rows gb) e)
     (for ([pos (in-list moves)])
       (gap-buffer-move-to! gb pos)
       (check-equal? (gap-buffer-rows gb) e))))

  (define ((test-list-input test-rows-inner)
           li [movesf (λ (i) empty)] [add-to-last? #t])
    (define i
      (string-append*
       (if add-to-last?
         (map (λ (s) (string-append s "\n")) li)
         (add-between li "\n"))))
    (define e
      (cond
        [(empty? li)
         1]
        [(and (not add-to-last?) (string=? "" (last li)))
         (max 1 (sub1 (length li)))]
        [else
         (length li)]))
    (define moves (movesf i))
    (test-rows-inner i moves e (vector add-to-last? li)))

  (define test-rows (test-list-input test-rows-inner))

  (define (random-string-moves s)
    (random-moves (string-length s)))

  (test-rows empty)
  (test-rows '("") (λ (s) empty) #f)
  (test-rows '("G" "KXJKKJQL" "SC" "IAYAS" "") (λ (s) empty) #f)
  (for* ([i (in-range N)]
         [? (in-list '(#t #f))])
    (test-rows (random-list-input) random-string-moves ?)))

(define (gap-buffer-row-ranges gb)
  (match-define (gap-buffer buf size pre post) gb)
  (define last 0)
  (define past-gap? #f)
  (for/list ([actual-end (gap-buffer-newline-offsets gb)])
    (define end actual-end)
    (unless past-gap?
      (when (<= post end)
        (set! end (list pre post end))
        (set! past-gap? #t)))
    (begin0 (cons last end)
            (set! last actual-end))))

(define (gap-buffer-row-cols gb row)
  (match (list-ref (gap-buffer-row-ranges gb) row)
    [(list start mid-start mid-end end)
     (+ (- mid-start start) (- end mid-end))]
    [(cons start end)
     (sub1 (- end start))]))

;; xxx gap-buffer-row-cols

(module+ test

  (define (test-row-cols-inner i moves rows orig)
    (match-define (vector ? li) orig)
    (define gb (string->gap-buffer i))
    (define (check)
      (for ([row (in-range rows)])
        (check-equal? (gap-buffer-row-cols gb row)
                      (if (empty? li) 0 (string-length (list-ref li row)))
                      (format "row ~a" row))))
    (test-case
     (format "test-row-cols ~v ~v ~v ~v" i moves rows orig)
     (check)
     (for ([pos (in-list moves)])
       (gap-buffer-move-to! gb pos)
       (check))))

  (define test-row-cols (test-list-input test-row-cols-inner))

  (test-row-cols empty (λ (s) empty) #f)
  (test-row-cols '("") (λ (s) '(0)) #f)
  (test-row-cols '("J" "") (λ (s) '()) #t)
  (test-row-cols '("C" "VO" "LFRLNYUAJ" "IMDY" "BWO")
                 (λ (s) '(14 14 1 4 5 11 17 23 0 21))
                 #t)
  (exit 1)

  (for* ([i (in-range N)]
         [? (in-list '(#t #f))])
    (test-row-cols (random-list-input) random-string-moves ?)))

;; xxx gap-buffer-row
;; xxx gap-buffer-move-to-row&col!

(define (gap-buffer-row gb row)
  (match-define (gap-buffer buf _ _ _) gb)
  (match (list-ref (gap-buffer-row-ranges gb) row)
    [(list start mid-start mid-end end)
     (string-append (substring buf start mid-start)
                    (substring buf mid-end end))]
    [(cons start end)
     (substring buf start end)]))

(define (gap-buffer-move-to/rc! gb row col)
  (debugf "moving to ~a,~a\n" row col)
  (match (list-ref (gap-buffer-row-ranges gb) row)
    [(list start mid-start mid-end end)
     (define before-mid (- mid-start start))
     (debugf "line split across gap: ~e\n"
             (list start mid-start mid-end end before-mid))
     (cond
       [(< col before-mid)
        (debugf "before mid\n")
        (gap-buffer-move-to! gb (+ start col 1))]
       [else
        (debugf "after mid\n")
        (gap-buffer-move-to! gb (+ mid-start (- col before-mid)))])]
    [(cons start end)
     (gap-buffer-move-to! gb (+ start col))]))

(provide (all-defined-out))
