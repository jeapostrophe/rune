#lang racket/base
(require (for-syntax racket/base
                     racket/syntax
                     racket/dict
                     racket/list
                     syntax/stx
                     syntax/id-table
                     syntax/parse)
         racket/string
         racket/package
         racket/match
         racket/list
         ffi/unsafe
         ffi/cvector
         ffi/vector
         ffi/unsafe/cvector
         web-server/templates
         opengl
         opengl/tree)

(define (print-bandwidth label ctype)
  (define DrawnMult 6)
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

(define-syntax (define-opengl-struct stx)
  (syntax-parse stx
    [(_ n:id ([f:id ty:expr] ...))
     (with-syntax*
      ([_n (format-id #'n "_~a" #'n)]
       [GL_n (format-id #'n "GL_~a" #'n)]
       [(fi ...) (build-list (length (syntax->list #'(f ...))) (λ (x) x))])
      (syntax/loc stx
        (begin
          (define-syntax GL_n
            (make-immutable-hasheq
             (list (cons 'f fi) ...)))
          (define-cstruct _n ([f ty] ...))
          (module+ test
            (print-bandwidth 'n _n)))))]))

(define-syntax (glstruct-offset stx)
  (syntax-parse stx
    [(_ cs:id f:id)
     (with-syntax*
      ([fi (dict-ref
            (syntax-local-value (format-id #'cs "GL_~a" #'cs))
            (syntax-e #'f))])
      (syntax/loc stx
        fi))]))

(define-syntax (GL_TEXTUREi stx)
  (syntax-parse stx
    [(_ n:nat)
     (format-id #'GL_TEXTURE0 "GL_TEXTURE~a" (syntax-e #'n))]))

(define ctype-name->bytes
  (match-lambda
   ['uint8 1]
   ['int8 1]
   ['int16 2]
   ['uint16 2]
   ['int32 4]
   ['uint32 4]
   ['float 4]))
(define (sum l)
  (apply + l))
(define (ctype-offset _type offset)
  (sum (map ctype-name->bytes (take (ctype->layout _type) offset))))

(define (sublist l s e)
  (for/list ([x (in-list l)]
             [i (in-naturals)]
             #:when (<= s i)
             #:when (<= i e))
    x))

(define (list-only l)
  (define v (first l))
  (for ([x (in-list (rest l))])
    (unless (eq? v x) (error 'list-only "List is not uniform: ~e" l)))
  v)

(define (ctype-range-type _type s e)
  (list-only (sublist (ctype->layout _type) s e)))

(define ctype->gltype
  (match-lambda
   ['uint8 (values #t GL_UNSIGNED_BYTE)]
   ['int8 (values #t GL_BYTE)]
   ['uint16 (values #t GL_UNSIGNED_SHORT)]
   ['int16 (values #t GL_SHORT)]
   ['uint32 (values #t GL_UNSIGNED_INT)]
   ['int32 (values #t GL_INT)]
   ['float (values #f GL_FLOAT)]))

(define (print-shader-log glGetShaderInfoLog shader-name shader-id [source #f])
  (define-values (infoLen infoLog)
    (glGetShaderInfoLog shader-id 1024))
  (unless (zero? infoLen)
    (for ([i (in-naturals)]
          [l (in-list (string-split source "\n"))])
      (eprintf "~a: ~a\n" i l))
    (printf "\n")
    (eprintf "~a:\n~a\n"
             shader-name
             (subbytes infoLog 0 infoLen))
    (exit 1)))

(define-syntax-rule
  (define&compile-shader VertexShaderId
    GL_VERTEX_SHADER
    ProgramId VertexShader)
  (begin (define VertexShaderId (glCreateShader GL_VERTEX_SHADER))
         (define ShaderSource
           (string-append* (flatten VertexShader)))
         (glShaderSource VertexShaderId 1
                         (vector ShaderSource)
                         (s32vector))
         (glCompileShader VertexShaderId)
         (print-shader-log
          glGetShaderInfoLog '(ProgramId VertexShaderId) VertexShaderId
          ShaderSource)
         (glAttachShader ProgramId VertexShaderId)))

(define (set-opengl-texture!* which tv)
  (glActiveTexture which)
  (glBindTexture GL_TEXTURE_2D tv))
(define-syntax-rule (set-opengl-texture! tni tv)
  (set-opengl-texture!* (GL_TEXTUREi tni) tv))

(define (glUniform* ProgramId name v)
  (define loc (glGetUniformLocation ProgramId name))
  (cond
    [(and (number? v) (exact? v))
     (glUniform1i loc v)]
    [(and (number? v) (inexact? v))
     (glUniform1f loc v)]
    [(f32vector? v)
     (glUniform2f loc (f32vector-ref v 0) (f32vector-ref v 1))]
    [else
     (error 'glUniform* "can't deal with ~e type" v)]))

(begin-for-syntax
  (define-syntax-class dynopt
    #:attributes (static dynamic)
    (pattern (#:uniform un:id uv:expr)
             #:attr static
             ;; xxx add static cache
             #'()
             #:attr dynamic
             (λ (ProgramId duns i)
               (define dun
                 (or (for/or ([dun (in-list (syntax->list duns))])
                       ;; xxx
                       (and (eq? (syntax-e dun) (syntax-e #'un))
                            dun))
                     (error 'dynopt-uniform "Can't find ~v in ~v\n" #'un duns)))
               (quasisyntax/loc #'un
                 (begin (glUniform* #,ProgramId  #,dun uv)
                        #,i))))
    (pattern (#:texture tni:nat tv:expr)
             #:attr static
             #'()
             #:attr dynamic
             (λ (ProgramId duns i)
               (quasisyntax/loc #'tni
                 (begin (set-opengl-texture! tni tv)
                        #,i
                        (set-opengl-texture! tni 0)))))
    (pattern (#:when cond:expr o:dynopt)
             #:attr static
             (attribute o.static)
             #:attr dynamic
             (λ (ProgramId duns i)
               (quasisyntax/loc #'cond
                 (begin (when cond
                          #,((attribute o.dynamic) ProgramId duns #'(void)))
                        #,i))))))

(define-syntax (define-opengl-program stx)
  (syntax-parse stx
    [(_ p:id
        (~seq #:struct cs:id)
        (~seq #:vertex-spec vh vv)
        (~seq #:uniform un:id uv:expr) ...
        (~seq #:dynuniform dun:id) ...
        (~seq #:texture tni:nat tv:expr) ...
        (~seq #:attribute an:id (af:id ...)) ...
        (~seq #:connected cn:id) ...
        (~seq #:vertex vs:expr)
        (~seq #:fragment fs:expr))
     (with-syntax*
      ([_cs (format-id #'cs "_~a" #'cs)]
       [with-p (format-id #'p "with-~a" #'p)]
       [inner-p (format-id #'p "inner-~a" #'p)]
       [set-vh! (format-id #'vh "set-~a-~a!" #'cs #'vh)]
       [set-vv! (format-id #'vv "set-~a-~a!" #'cs #'vv)]
       [(ai ...) (build-list (length (syntax->list #'(an ...))) (λ (x) x))]
       [((as ae) ...)
        (for/list ([afss (in-list (syntax->list #'((af ...) ...)))])
          (define afs (syntax->list afss))
          (list (first afs) (last afs)))]
       [(rtni ...) (reverse (syntax->list #'(tni ...)))])
      (syntax/loc stx
        (begin
          (define-package this-program (p with-p inner-p)
            (define ProgramData-count
              0)
            (define ProgramData #f)

            (define (install! i o)
              (define-syntax-rule (point-install! Horiz Vert j (... ...))
                (begin
                  (set-vh! o Horiz)
                  (set-vv! o Vert)
                  (cvector-set! ProgramData (+ (* i DrawnMult) j) o)
                  (... ...)))
              (point-install! -0 +1 0)
              (point-install! +1 +1 1 4)
              (point-install! -0 -0 2 3)
              (point-install! +1 -0 5))

            (define ProgramId (glCreateProgram))

            (define un (symbol->string (gensym 'un)))
            ...
            (define dun (symbol->string (gensym 'dun)))
            ...
            (define an (symbol->string (gensym 'an)))
            ...
            (define cn (symbol->string (gensym 'cn)))
            ...

            (glBindAttribLocation ProgramId ai an)
            ...

            (define&compile-shader VertexShaderId GL_VERTEX_SHADER
              ProgramId vs)
            (define&compile-shader FragmentShaderId GL_FRAGMENT_SHADER
              ProgramId fs)

            (define *initial-count*
              (* 2 512))

            (glLinkProgram ProgramId)
            (print-shader-log glGetProgramInfoLog 'Program ProgramId)

            (glUseProgram ProgramId)
            (glUniform* ProgramId un uv)
            ...
            (glUseProgram 0)

            (define VaoId
              (u32vector-ref (glGenVertexArrays 1) 0))
            (glBindVertexArray VaoId)

            (define (glVertexAttribIPointer* index size type normalized stride pointer)
              (glVertexAttribIPointer index size type stride pointer))

            (define-syntax-rule
              (define-vertex-attrib-array
                Index ProgramData-start-id ProgramData-end-id)
              (begin
                (define ProgramData-start (glstruct-offset cs ProgramData-start-id))
                (define ProgramData-end (glstruct-offset cs ProgramData-end-id))
                (define-values (int? type)
                  (ctype->gltype
                   (ctype-range-type _cs ProgramData-start ProgramData-end)))
                (define byte-offset
                  (ctype-offset _cs ProgramData-start))
                (define HowMany
                  (add1 (- ProgramData-end ProgramData-start)))
                ((if int? glVertexAttribIPointer* glVertexAttribPointer)
                 Index HowMany type
                 #f
                 (ctype-sizeof _cs)
                 byte-offset)
                (glEnableVertexAttribArray Index)))

            (define VboId
              (u32vector-ref (glGenBuffers 1) 0))
            (glBindBuffer GL_ARRAY_BUFFER VboId)
            (define-vertex-attrib-array ai as ae)
            ...
            (glBindBuffer GL_ARRAY_BUFFER 0)

            (glBindVertexArray 0)

            (define-syntax (p stx)
              (syntax-parse stx
                [(_ o:dynopt (... ...) es:expr)
                 (syntax/loc stx
                   (with-p o (... ...) (inner-p es)))]))

            (define-syntax (with-p stx)
              (syntax-parse stx
                [(_ o:dynopt (... ...) ie:expr (... ...))
                 (syntax/loc stx
                   (begin
                     (glBindVertexArray VaoId)
                     (glEnableVertexAttribArray ai)
                     ...
                     (set-opengl-texture! tni tv)
                     ...
                     (glUseProgram ProgramId)
                     (with-dynopts o (... ...)
                                   (let () ie (... ...)))
                     (glUseProgram 0)
                     (set-opengl-texture! rtni 0)
                     ...))]))

            (define-syntax (inner-p stx)
              (syntax-parse stx
                [(_ o:dynopt (... ...) es:expr)
                 (syntax/loc stx
                   (with-dynopts o (... ...) (inner-p* es)))]))

            (define-syntax (with-dynopts stx)
              (syntax-parse stx
                [(_ ie:expr)
                 #'ie]
                [(_ o1:dynopt o:dynopt (... ...) ie:expr)
                 ((attribute o1.dynamic)
                  #'ProgramId
                  #'(dun ...)
                  (syntax/loc stx
                    (with-dynopts o (... ...) ie)))]))

            (define DrawType GL_TRIANGLES)
            (define DrawnMult 6)
            (define (inner-p* es)
              (define drawn-count (tree-count es))
              (define ProgramData-count:new (max *initial-count* drawn-count))

              (glBindBuffer GL_ARRAY_BUFFER VboId)

              (unless (>= ProgramData-count ProgramData-count:new)
                (define ProgramData-count:old ProgramData-count)
                (set! ProgramData-count
                      (max (* 2 ProgramData-count)
                           ProgramData-count:new))
                (glBufferData GL_ARRAY_BUFFER
                              (* ProgramData-count
                                 DrawnMult
                                 (ctype-sizeof _cs))
                              #f
                              GL_STREAM_DRAW))

              (set! ProgramData
                    (make-cvector*
                     (glMapBufferRange
                      GL_ARRAY_BUFFER
                      0
                      (* ProgramData-count
                         DrawnMult
                         (ctype-sizeof _cs))
                      (bitwise-ior
                       ;; We are overriding everything (this would be wrong if
                       ;; we did the caching "optimization" I imagine)
                       GL_MAP_INVALIDATE_RANGE_BIT
                       GL_MAP_INVALIDATE_BUFFER_BIT

                       ;; We are not doing complex queues, so don't block other
                       ;; operations (but it doesn't seem to improve performance
                       ;; by having this option)
                       ;; GL_MAP_UNSYNCHRONIZED_BIT

                       ;; We are writing
                       GL_MAP_WRITE_BIT))
                     _cs
                     (* ProgramData-count
                        DrawnMult)))

              (tree-iter! install! es)
              (glUnmapBuffer GL_ARRAY_BUFFER)
              (glBindBuffer GL_ARRAY_BUFFER 0)

              (glDrawArrays
               DrawType 0
               (* DrawnMult drawn-count))))
          (open-package this-program))))]))

(define GLSL-Library
  (include-template "program.glsl"))

(provide
 define-opengl-program
 define-opengl-struct
 GLSL-Library
 _float _uint8 _sint8 _uint16 _uint32)
