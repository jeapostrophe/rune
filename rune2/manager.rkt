#lang racket/base

;; I'm 70% sure I don't want these
      [(event:uzbl (or 'bot 'top)
                   (or 'LOAD_START
                       'REQUEST_STARTING
                       'VARIABLE_SET
                       'LOAD_COMMIT
                       'TITLE_CHANGED
                       'LOAD_PROGRESS
                       'LOAD_FINISH
                       'SCROLL_HORIZ
                       'SCROLL_VERT)
                   _)
       (void)]
