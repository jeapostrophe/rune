#lang racket/base
(require opengl
         ffi/unsafe)
(module+ test
  (require rackunit))

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
   [row _uint16] ;; 65k lines should be enough for anyone
   [col _uint8] ;; more than 80 cols is immoral anyways

   ;; or just 16 colors on each [solarized]
   [f*b _uint8]

   ;; which character? [I only found 118 unique characters in every
   ;; text document on my system, except for some Japanese notes and a
   ;; file of all unicode characters]
   [ci _uint8]

   ;; vertex id
   [horiz _sint8]
   [vert _sint8]))

(define DrawnMult 6)
(module+ test
  (define (print-bandwidth label ctype)
    (define vert-size (ctype-sizeof ctype))
    (eprintf "One ~a vert is ~a bytes\n" label vert-size)
    (eprintf "One ~a is ~a verts\n" label DrawnMult)
    (eprintf "One ~a is ~a bytes\n" label (* DrawnMult vert-size))
    (eprintf "One ~a @ 60 FPS is ~a bytes per second\n" label (* 60 DrawnMult vert-size))
    (eprintf "Intel HD Graphics 4000 would give ~a ~a at 60 FPS (considering only memory)\n"
             (real->decimal-string
              (/ (* 25.6 1024 1024 1024)
                 (* 60 DrawnMult vert-size)))
             label)
    (eprintf "\n"))

  (print-bandwidth "char" _char-info))

(define (nibbles hi lo)
  (+ (arithmetic-shift hi 4) lo))
(define (nibble-lo n)
  (bitwise-and n #x0F))
(define (nibble-hi n)
  (bitwise-and (arithmetic-shift n -4) #x0F))

(module+ test
  (for* ([x (in-range 16)]
         [y (in-range 16)])
    (define n (nibbles x y))
    (check-pred byte? n)
    (check-equal? (nibble-hi n) x)
    (check-equal? (nibble-lo n) y)))

(define-cstruct _block-info
  (;; where it is on the screen (screens are relatively small)
   [x _uint16]
   [y _uint16]
   ;; what is the width/height available
   [w _uint16]
   [h _uint16]
   ;; draw a box around it?
   [box-color _uint8]
   ;; x-offset
   [dx _uint16]
   ;; which buffer should be displayed
   [tex _uint32]))

(module+ test
  (print-bandwidth "block" _block-info)

  ;; 1. Using individual textures is obvious. It would be "slow"
  ;; because I would rebind for every window.

  ;; 2. If I used a single 2DTextureArray I could support all buffers
  ;; at the same time, but this would be a problem, because I don't
  ;; think I can mandate that, for instance, other X11 windows will
  ;; draw into my texture. In addition, they would all have to be the
  ;; same size, so I'd waste space and have painful rebinding.

  ;; 3. One idea is to divide the M textures into N groups of size
  ;; GL_MAX_COMBINED_TEXTURE_IMAGE_UNITS and make N draw calls. I
  ;; could do this, if I could have the vertex shader use a dynamic
  ;; reference (from the block-info) to figure out what the sampler
  ;; was from a large uniform array. Unfortunately, this is not
  ;; possible because a uniform array reference has to be constant.

  ;; 4. I could divide the M textures into N groups of size
  ;; GL_MAX_COMBINED_TEXTURE_IMAGE_UNITS. I would then make M calls to
  ;; glUniformi to set what the current texture was and make M calls
  ;; to draw. When N=1, then this is very fast because I never need to
  ;; do any bind operations. When N>1, then this is really equivalent
  ;; to 1, because I need to rebind every texture every frame.  A blog
  ;; (http://outerra.blogspot.com/2012/11/opengl-notes-2-texture-bind-performance.html)
  ;; suggests this would be very fast. On my machine this is
  ;; 32... which is quite small.

  (printf "Min possible is ~a\n" 80)
  (printf "Max possible is ~a\n" GL_MAX_COMBINED_TEXTURE_IMAGE_UNITS)
  (require ffi/vector
           racket/class
           racket/gui/base)

  (let ()
    (define frame
      (new frame%
           [label ""]
           [style '(hide-menu-bar
                    no-resize-border
                    no-caption
                    no-system-menu)]))
    (define this-canvas%
      (class canvas%
        (define/override (on-paint)
          (define dc (send this get-dc))
          (define glctx (send dc get-gl-context))
          (send glctx call-as-current
                (λ ()
                  (printf "Actual is ~a\n"
                          (s32vector-ref (glGetIntegerv GL_MAX_COMBINED_TEXTURE_IMAGE_UNITS) 0))
                  (exit 1))))

        (super-new)))

    (define config
      (new gl-config%))
    (send config set-double-buffered #t)

    (define canvas
      (new this-canvas%
           [parent frame]
           [gl-config config]
           [style '(gl no-autoclear)]))

    (send frame show #t)
    (send canvas focus)
    (send canvas refresh-now)))
