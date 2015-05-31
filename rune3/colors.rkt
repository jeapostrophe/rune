#lang racket/base
(require racket/match
         (prefix-in c: mode-lambda/color))

(define COLORS 16)

(match-define
  (list UI-LO UI-MI FG-HI FG-MI UI-HI FG-LO BG-HI BG
        YELLOW ORANGE RED MAGENTA VIOLET BLUE CYAN GREEN)
  (build-list COLORS (λ (i) i)))

(define (colors . args)
  (define argv (list->vector args))
  (define the-colors (make-vector COLORS #f))
  (for ([i (in-range (/ (vector-length argv) 4))])
    (define c (vector-ref argv (+ (* 4 i) 0)))
    (define r (vector-ref argv (+ (* 4 i) 1)))
    (define g (vector-ref argv (+ (* 4 i) 2)))
    (define b (vector-ref argv (+ (* 4 i) 3)))
    (vector-set! the-colors c (c:argb 255 r g b)))
  the-colors)

(define (colors-ref c r)
  (vector-ref c r))

(define solarized-light
  (colors
   UI-LO     #x00 #x2B #x36
   UI-MI     #x07 #x36 #x42
   FG-HI     #x58 #x6E #x75
   FG-MI     #x65 #x7B #x83
   UI-HI     #x83 #x94 #x96
   FG-LO     #x93 #xA1 #xA1
   BG-HI     #xEE #xE8 #xD5
   BG        #xFD #xF6 #xE3
   YELLOW    #xB5 #x89 #x00
   ORANGE    #xCB #x4B #x16
   RED       #xDC #x32 #x2F
   MAGENTA   #xD3 #x36 #x82
   VIOLET    #x6C #x71 #xC4
   BLUE      #x26 #x8B #xD2
   CYAN      #x2A #xA1 #x98
   GREEN     #x85 #x99 #x00))

(define default-colors
  solarized-light)

(provide (all-defined-out))
