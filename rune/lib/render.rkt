#lang racket/base
(require ffi/unsafe)

;; Basic idea:

;; 1. Render each buffer to a texture using a shader optimized for
;; bi-color monospace characters

;; 2. Composite each texture into the whole screen image

;; Complications

;; 1. The only annoying part about this creating the texture
;; atlas/array texture for the characters. Luckily they are all the
;; same size, so layout is easy. If I use a texture array, are there
;; enough slots in it for all the usable characters? Should I scan the
;; file / all open files for which characters to include?

;; The other annyoing thing is resizing the texture when the buffer
;; gets more rows/cols

;; 2. Using the fixed-function pipeline for this is easy, but it
;; requires a lot of texture swapping. It doesn't seem wise to read
;; the textures back into the CPU to create a new atlas for every
;; frame, so the swapping is probably the best option. 

;; UNLESS, I can render to a texture array. But, can you render to a
;; texture array?  Are there enough for a very large number of
;; buffers? Would I pin all the buffers to the array as I was
;; generating the vertex data, so the limit is just on the number of
;; displayed? If we did that, then every buffer would have to be the
;; same size... what is a good size?

(define-cstruct _char-info
  (;; location (monospace = all w/h the same)
   [row _uint16]
   [col _uint16]

   ;; foreground & background colors
   [fr _uint8]
   [fg _uint8]
   [fb _uint8]
   [br _uint8]
   [bg _uint8]
   [bb _uint8]
   
   ;; which character?
   [ci _uint16]

   ;; vertex id
   [horiz _sint8]
   [vert _sint8]))

(define DrawnMult 6)
(module+ test
  (define vert-size (ctype-sizeof _char-info))
  (eprintf "One vert is ~a bytes\n" vert-size)
  (eprintf "One char is ~a verts\n" DrawnMult)
  (eprintf "One char is ~a bytes\n" (* DrawnMult vert-size))
  (eprintf "One char @ 60 FPS is ~a bytes per second\n" (* 60 DrawnMult vert-size))
  (eprintf "Intel HD Graphics 4000 would give ~a chars at 60 FPS (considering only memory)\n"
           (real->decimal-string
            (/ (* 25.6 1024 1024 1024)
               (* 60 DrawnMult vert-size)))))
