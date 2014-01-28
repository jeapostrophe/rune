#lang racket/base
(require racket/runtime-path
         racket/cmdline
         racket/match
         racket/file
         xml
         hirune/common
         hirune/util
         rune/lib/buffer)

(define HIRUNE-DIR "/tmp/hirune/html")

(define-runtime-path hirune.css "hirune.css")
(define-runtime-path hirune.js "hirune.js")

(define hirune-file-port (make-parameter #f))

(define (path->hirune-file-url p)
  (format "http://localhost:~a~a" (hirune-file-port) p))

(define (hirune-xexpr #:anchor [anchor ""]
                      xe)
  (define p (make-temporary-file "hirune-~a.html" #f HIRUNE-DIR))
  (with-output-to-file p
    #:exists 'replace
    (λ () (write-xexpr xe)))
  (command:hirune:update anchor (path->bytes p)))

(define (hirune-file #:anchor [anchor ""]
                     bxe)
  (hirune-xexpr
   #:anchor anchor ;; xxx ugh
   `(html
     (head
      (link ([rel "stylesheet"]
             [type "text/css"]
             [href ,(path->hirune-file-url hirune.css)]))
      (script ([src "//code.jquery.com/jquery-1.10.1.min.js"]) "")
      (script ([src ,(path->hirune-file-url hirune.js)]) ""))
     (body ([class "content1_fg bg_bg"])
           ,bxe))))

(define (hirune-file/lines ls)
  (hirune-file
   `(div ,@(for/list ([l (in-list ls)]
                      [i (in-naturals)])
             `(span ([class "line"] [id ,(format "line~a" i)])
                    ,l)))))

(define (hirune-file/buffer b)
  (hirune-file/lines (buffer->strings b)))

(define (hirune-file/buffer/cursor b row col)
  (hirune-file/buffer (buffer-insert-char b row col #\‸)))

(define (hirune #:label state-label
                #:opaque-state os
                #:command state=>/command
                #:render state-render)

  (command-line #:program "hirune"
                #:once-each
                ["--file-port" fps
                 "specify the port used by the hirune file server"
                 ;; xxx or look at env var
                 (hirune-file-port (string->number fps))])

  ;; xxx use label to signal existence

  ;; xxx don't assume stdin/out
  (define command-source (read-evt (current-input-port)))
  (let loop ([s os] [last #f])
    (unless (equal? s last)
      (writeln (state-render s)))
    (sync
     (handle-evt
      command-source
      (λ (ec)
        ;; xxx don't just die on bad events
        (match-define (event:hirune:command c) ec)
        (loop (state=>/command s c) s))))))

(provide (all-defined-out))