#lang racket/base
(require opengl
         racket/class
         ffi/vector)

(define (allocate-texture)
  (define t (u32vector-ref (glGenTextures 1) 0))
  (glBindTexture GL_TEXTURE_2D t)
  (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_NEAREST)
  (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_NEAREST)
  (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_CLAMP)
  (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_CLAMP)
  t)

;; Copied
(define (argb->rgba! pixels)
  (for ((i (in-range (/ (bytes-length pixels) 4))))
    (let* ((offset (* 4 i))
           (alpha (bytes-ref pixels offset))
           (red (bytes-ref pixels (+ 1 offset)))
           (green (bytes-ref pixels (+ 2 offset)))
           (blue (bytes-ref pixels (+ 3 offset))))
      (bytes-set! pixels offset (quotient (* alpha red) 255))
      (bytes-set! pixels (+ 1 offset) (quotient (* alpha green) 255))
      (bytes-set! pixels (+ 2 offset) (quotient (* alpha blue) 255))
      (bytes-set! pixels (+ 3 offset) alpha))))

(define (load-texture/bitmap bm #:texture [t (allocate-texture)])
  (define w (send bm get-width))
  (define h (send bm get-height))
  (define pixels (make-bytes (* w h 4)))

  (send bm get-argb-pixels 0 0 w h pixels)
  (argb->rgba! pixels)

  (glBindTexture GL_TEXTURE_2D t)
  (glTexImage2D GL_TEXTURE_2D 0 GL_RGBA8 w h 0 GL_RGBA GL_UNSIGNED_BYTE pixels)

  t)

(provide
 (all-defined-out))
