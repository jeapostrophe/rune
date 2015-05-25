#lang racket/base
(require racket/match
         rune/events
         rune/buffer)

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
    [(event:rune:key 'C-<left>)
     (editor mb 0)]
    [(event:rune:key '<left>)
     (editor mb (max 0 (sub1 mbc)))]
    [(event:rune:key 'C-<right>)
     (editor mb (buffer-row-cols mb 0))]
    [(event:rune:key '<right>)
     (editor mb (min (buffer-row-cols mb 0) (add1 mbc)))]
    [(event:rune:key (or '<backspace> 'S-<backspace>))
     (cond
       [(> mbc 0)
        (define-values (_ mbp) (buffer-delete-previous mb 0 mbc))
        (editor mbp (sub1 mbc))]
       [else
        s])]
    [(event:rune:key '<delete>)
     (cond
       [(< mbc (buffer-row-cols mb 0))
        (define-values (_ mbp) (buffer-delete-next mb 0 mbc))
        (editor mbp mbc)]
       [else
        s])]
    [(event:rune:key
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
