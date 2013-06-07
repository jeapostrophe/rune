#lang racket/base
(require racket/match
         racket/list)
(module+ test
  (require rackunit)
  (define N 2)
  (define M 5)
  (define (random-input)
    (build-string M (λ (i) (integer->char (+ 64 (random 26)))))))

(define debugf void)

(struct gap-buffer (buf buf-size gap-start gap-end) #:mutable)

(define gap-size 8)

(define (string->gap-buffer str)
  (define len (string-length str))
  (define new-len (+ len gap-size))
  (define new-str
    (build-string new-len
                  (λ (i) (integer->char (+ (char->integer #\0)
                                           (modulo i 10))))))
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

  (for ([i (in-range N)])
    (test-move-seq (random-input) (build-list N (λ (i) (random M))))))

(define (gap-buffer-insert! gb c)
  (match-define (gap-buffer buf buf-size gs ge) gb)
  ;; If they are equal, then the gap is empty
  (when (= gs ge)
    (gap-buffer-expand! gb))
  (define new-gs (gap-buffer-gap-start gb))
  (string-set! buf new-gs c)
  (eprintf "insert: Wrote ~a to ~a\n" c new-gs)
  (gap-buffer-gap-start++ gb))

(define (gap-buffer-insert-string! gb s)
  (for ([c (in-string s)])
    (gap-buffer-insert! gb c)))

(define (gap-buffer-expand! gb)
  (match-define (gap-buffer old-buf old-buf-size old-gs old-ge) gb)
  (define new-size (+ old-buf-size gap-size))
  (define new-buf (make-string new-size))
  (string-copy! new-buf gap-size old-buf)
  (set-gap-buffer-buf! gb new-buf)
  (set-gap-buffer-buf-size! gb new-size)
  (set-gap-buffer-gap-start! gb 0)
  (set-gap-buffer-gap-end! gb gap-size))

;; xxx optimize?
(define (regexp-match-count rx input start-pos [end-pos #f])
  (length (regexp-match-positions* rx input start-pos end-pos)))

(define newline-rx #rx"\n")
(define (gap-buffer-newline-offsets gb)
  (match-define (gap-buffer buf size pre post) gb)
  (define before (regexp-match-positions* newline-rx buf 0 pre))
  (define after (regexp-match-positions* newline-rx buf post))
  (if (and (empty? before) (empty? after))
    ;; xxx fix
    (error 'gap-buffer-newline-offsets "No newline!")
    (map car (append before after))))

(define (gap-buffer-line-count gb)
  (length (gap-buffer-newline-offsets gb)))

(define (gap-buffer-line-ranges gb)
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

(define (gap-buffer-line-cols gb line)
  (match (list-ref (gap-buffer-line-ranges gb) line)
    [(list start mid-start mid-end end)
     (sub1 (+ (- mid-start start) (- end mid-end)))]
    [(cons start end)
     (sub1 (- end start))]))

(define (gap-buffer-lines gb)
  (match-define (gap-buffer buf _ _ _) gb)
  (for/list ([range (in-list (gap-buffer-line-ranges gb))])
    (match range
      [(list start mid-start mid-end end)
       (string-append (substring buf start mid-start)
                      (substring buf mid-end end))]
      [(cons start end)
       (substring buf start end)])))

(define (gap-buffer-move-to/rc! gb row col)
  (debugf "moving to ~a,~a\n" row col)
  (match (list-ref (gap-buffer-line-ranges gb) row)
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
