#lang racket/base
(require opengl
         ffi/vector
         racket/match
         opengl/texture)

(struct render-texture (t rb fbo [w #:mutable] [h #:mutable]))

(define (allocate-render-texture w h)
  (define t (allocate-texture))
  (define rb (u32vector-ref (glGenRenderbuffers 1) 0))
  (define fbo (u32vector-ref (glGenFramebuffers 1) 0))
  (define rt (render-texture t rb fbo 0 0))
  (resize-render-texture! rt w h))

(define (resize-render-texture! rt w h)
  (match-define (render-texture t rb fbo _ _) rt)
  (glBindTexture GL_TEXTURE_2D t)
  (glTexImage2D
   GL_TEXTURE_2D 0 GL_RGBA8 w h 0
   GL_RGBA GL_UNSIGNED_BYTE
   0)

  (glBindRenderbuffer GL_RENDERBUFFER rb)
  (glRenderbufferStorage GL_RENDERBUFFER
                         GL_DEPTH_COMPONENT24
                         w h)
  (glBindRenderbuffer GL_RENDERBUFFER 0)


  (glBindFramebuffer GL_FRAMEBUFFER fbo)
  (glFramebufferTexture2D
   GL_DRAW_FRAMEBUFFER
   GL_COLOR_ATTACHMENT0
   GL_TEXTURE_2D t 0)

  (glFramebufferRenderbuffer
   GL_FRAMEBUFFER
   GL_DEPTH_ATTACHMENT
   GL_RENDERBUFFER rb)

  (glBindFramebuffer GL_FRAMEBUFFER 0)

  (set-render-texture-w! rt w)
  (set-render-texture-h! rt h)

  rt)

(define-syntax-rule (with-texture-to-render rt . e)
  (match rt
    [(render-texture _ _ fbo w h)
     (glBindFramebuffer GL_FRAMEBUFFER fbo)
     (glViewport 0 0 w h)
     (let () . e)
     (glBindFramebuffer GL_FRAMEBUFFER 0)]))

(provide
 (all-defined-out))
