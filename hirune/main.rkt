#lang racket/base

(define (hirune-file #:anchor [anchor #f]
                     xe)
  ;; xxx save xe to file
  ;; xxx delete the old file
  ;; xxx signal outward to navigate to the url
  #f)

(define (hirune #:label state-label
                #:opaque-state os
                #:command state=>/command
                #:render state-render)
  ;; xxx look at env vars or command-line args for connection to manager

  ;; xxx connect to manager and signal that you exist

  ;; xxx as events/commands come in, update the state and potentially
  ;; send back updated pages to display
  
  #f)

(provide hirune
         hirune-file)
