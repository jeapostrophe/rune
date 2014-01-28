#lang racket/base
(require racket/runtime-path
         racket/file
         xml
         rune/lib/buffer)

(define HIRUNE-DIR "/tmp/hirune/html")

(define-runtime-path hirune.css "hirune.css")
(define-runtime-path hirune.js "hirune.js")

(define hirune-file-port (make-parameter #f))

(define (path->hirune-file-url p)
  (format "http://localhost:~a~a" (hirune-file-port) p))

(struct hirune-update (after xe) #:prefab)

(define (hirune-xexpr #:anchor [anchor ""]
                      xe)
  (define p (make-temporary-file "hirune-~a.html" #f HIRUNE-DIR))
  (with-output-to-file p
    #:exists 'replace
    (λ () (write-xexpr xe)))
  (hirune-update anchor (path->bytes p)))

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
  ;; xxx look at env vars or command-line args for connection to manager

  ;; xxx connect to manager and signal that you exist

  ;; xxx as events/commands come in, update the state and potentially
  ;; send back updated pages to display

  ;; xxx init hirune-file-port

  #f)

(provide (all-defined-out))
