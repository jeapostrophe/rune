#lang racket/base
(require racket/runtime-path
         racket/cmdline
         racket/match
         racket/file
         xml
         hirune/common
         hirune/util)

(define HIRUNE-DIR "/tmp/hirune/html")

(define-runtime-path hirune.css "static/hirune.css")
(define-runtime-path hirune.js "static/hirune.js")

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

  ;; xxx don't assume stdin/out
  (define command-source (read-evt (current-input-port)))
  (let loop ([s os] [last #f])
    (unless (equal? s last)
      (writeln (command:hirune:label (state-label s)))
      (writeln (state-render s)))
    (sync
     (handle-evt
      command-source
      (λ (ec)
        (unless (eof-object? ec)
          (define sp
            (match ec
              [(event:hirune:command c)
               (state=>/command s c)]
              [_
               s]))
          (loop sp s)))))))

(provide (all-defined-out))
