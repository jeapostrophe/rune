#lang racket/base
(require racket/match
         hirune/util
         hirune/common
         rune/lib/buffer)

(define-match-expander bind
  (syntax-rules ()
    [(_ id e)
     (app (λ (_) e) id)]))

(struct editor (buf col) #:transparent)

(define (make-editor)
  (editor (string->buffer "") 0))

(define (editor->string e)
  (buffer->string (editor-buf e)))

(define (editor-process s e)
  (match-define (editor mb mbc) s)
  (match e
    [(event:hirune:key 'C-<left>)
     (editor mb 0)]
    [(event:hirune:key '<left>)
     (editor mb (max 0 (sub1 mbc)))]
    [(event:hirune:key 'C-<right>)
     (editor mb (buffer-row-cols mb 0))]
    [(event:hirune:key '<right>)
     (editor mb (min (buffer-row-cols mb 0) (add1 mbc)))]
    [(event:hirune:key (or '<backspace> 'S-<backspace>))
     (cond
       [(> mbc 0)
        (define-values (_ mbp) (buffer-delete-previous mb 0 mbc))
        (editor mbp (sub1 mbc))]
       [else
        s])]
    [(event:hirune:key '<delete>)
     (cond
       [(< mbc (buffer-row-cols mb 0))
        (define-values (_ mbp) (buffer-delete-next mb 0 mbc))
        (editor mbp mbc)]
       [else
        s])]
    [(event:hirune:key
      (or (? char? c)
          (and (or 'S-<space> '<space>)
               (bind c #\space))))
     (define mbp (buffer-insert-char mb 0 mbc c))
     (editor mbp (add1 mbc))]
    [e
     (writeln `(event ,e))
     s]))

(provide
 editor?
 make-editor
 editor->string
 editor-col
 editor-process)
