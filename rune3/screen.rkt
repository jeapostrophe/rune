#lang racket/base
(require racket/match
         rune/colors)

(struct cell (fg bg c))
(struct screen (rows cols row-vector))
(define default-cell #f)

(define (make-screen #:rows rows #:cols cols)
  (screen rows cols
          (build-vector rows
                        (λ (rowi)
                          (make-row #:cols cols)))))
(define (make-row #:cols cols)
  (make-vector cols default-cell))

(define (screen-write-char! sc row col c)
  (match-define (screen rows cols row-vector) sc)
  (when (and (< row rows) (< col cols))
    (vector-set! (vector-ref row-vector row) col c)))
(define (screen-copy! source dest)
  (for ([row (in-vector (screen-row-vector source))]
        [rowi (in-naturals)])
    (for ([c (in-vector row)]
          [coli (in-naturals)])
      (screen-write-char! dest rowi coli c))))

(define (cmd? x)
  (or (cmd:posn? x)
      (cmd:move? x)
      (cmd:fg? x)
      (cmd:bg? x)
      (cmd:row? x)
      (cmd:col? x)
      (char? x)))
(struct cmd:posn (row col cmd) #:prefab)
(struct cmd:move (row col cmd) #:prefab)
(struct cmd:fg (fg cmd) #:prefab)
(struct cmd:bg (bg cmd) #:prefab)
(struct cmd:row (cmds) #:prefab)
(struct cmd:col (cmds) #:prefab)

(struct layout (row col rows cols) #:prefab)
(struct cmd:bounds (lay cmd) #:prefab)

(struct cmds:repeat (i cmd) #:prefab)

(define (enforce-layout lay output!)
  (match-define (layout lrow lcol rows cols) lay)
  (λ (rowi coli cell)
    (when (and (< rowi rows) (< coli cols))
      (output! (+ lrow rowi) (+ lcol coli) cell))))

(define (screen-write! sc cmd)
  (let loop ([rowi 0] [coli 0]
             [fg FG-MI] [bg BG]
             [output!
              (λ (rowi coli cell)
                (screen-write-char! sc rowi coli cell))]
             [cmd cmd])
    (define (loops dc dr cmds)
      (for ([cmd (match cmds
                   [(? list?)
                    (in-list cmds)]
                   [(? string?)
                    (in-string cmds)]
                   [(cmds:repeat i cmd)
                    (in-list (build-list i (λ (i) cmd)))])]
            [i (in-naturals)])
        (loop (+ (* i dr) rowi) (+ (* i dc) coli) fg bg output! cmd)))
    (match cmd
      [(cmd:bounds lay cmd)
       (loop rowi coli fg bg (enforce-layout lay output!) cmd)]
      [(cmd:posn rowi coli cmd)
       (loop rowi coli fg bg output! cmd)]
      [(cmd:move drowi dcoli cmd)
       (loop (+ drowi rowi) (+ dcoli coli) fg bg output! cmd)]
      [(cmd:fg fg cmd)
       (loop rowi coli fg bg output! cmd)]
      [(cmd:bg bg cmd)
       (loop rowi coli fg bg output! cmd)]
      [(? char? c)
       (output! rowi coli (cell fg bg c))]
      [(cmd:row cmds)
       (loops +1 0 cmds)]
      [(cmd:col cmds)
       (loops 0 +1 cmds)])))

(provide (all-defined-out))
