#lang racket/base
(require racket/file
         data/integer-set
         racket/match)

(define (read-record p)
  (match-define (list 'record n isc) (file->value p))
  (count (make-integer-set isc)))

(define (write-record p r)
  (match-define (record n is) r)
  (write-to-file (list 'record n (integer-set-contents is))
                 p
                 #:exists 'replace))

(define (merge-file rp r p)
  (match-define (record n is) r)
  (define k (count is))
  (define new-is
    (call-with-input-file p
      (λ (ip)
        (for/fold ([is is])
            ([c (in-port read-char ip)])
          (union is (make-range (char->integer c)))))))
  (define new-k (count new-is))
  (define new-r (record (add1 n) new-is))
  (write-record rp new-r)
  (unless (= k new-k)
    (printf "~a: ~a -> ~a\n" p k new-k)
    (flush-output))
  new-r)

(struct record (n is))
(define empty-record (record 0 (make-range)))

(define (go! record-p root)
  (fold-files
   (match-lambda*
    [(list p 'dir r)
     r]
    [(list (and p (app path->string ps)) (or 'link 'file) r)
     (cond
       [(regexp-match #rx"(rkt|scrbl|tex)$" ps)
        (merge-file record-p r p)]
       [else
        r])])
   empty-record
   root))

(module+ main
  (require racket/runtime-path)
  (define-runtime-path record "record")
  (if #t
    (go! record (find-system-path 'home-dir))
    (read-record record)))
