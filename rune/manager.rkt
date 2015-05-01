#lang racket/base
(require rune/comm
         racket/match
         gfx/color)

(module+ main
  (define l (comm-listener 'viewer->manager))
  (define s (comm-sender 'manager->viewer))

  (let loop ([rs 1] [cs 1])
    (define-values (nrs ncs)
      (match (sync l)
        [(comm:viewer>:size nrs ncs)
         (values nrs ncs)]
        [_
         (values rs cs)]))
    (s (comm:>viewer:bg! (argb 255 255 0 0)))
    (s (comm:>viewer:write!
        (random nrs) (random ncs)
        (cell
         (argb 0 (random 255) (random 255) (random 255))
         (argb 0 (random 255) (random 255) (random 255))
         (integer->char
          (+ (char->integer #\a) (random 26))))))
    (loop nrs ncs)))
