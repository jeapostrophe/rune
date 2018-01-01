#lang racket/base
(require racket/generic
         racket/list
         racket/serialize)

;; XXX My idea has evolved. I'm thinking now of having an object with
;; interactable commands that can be introspected. The UI can then
;; automatically generate a REPL/command buffer, but also you can set
;; up key-bindings to them. These are arranged in a hierarchy and
;; there's a delegation structure as well. Then the object render out
;; their state and this is assembled up the chain into a UI. The
;; rendering is abstract so it can be put in a text or WWW.
;;
;; Initially, I expect that this will essentially be a window manager
;; for terminal emulators and I'll make all the commands correspond to
;; the original key inputs of the programs I'll be running, so I can
;; rebind them to be all consistent. (Some programs don't have
;; configuration files/etc.)
;;
;; Questions
;;
;; - How to figure out what commands should be available (All commands
;; + delegates, recursively? --- What happens when a delegate receives
;; the same command as something higher up, does it always get masked?
;; Maybe objects should be in an explicit tree with paths.)
;;
;; - How to pull out the commands from deep down and give them
;; keybindings? (Obvious way to implement is that only the top-level
;; knows about key-bindings, but then the top-level needs to know
;; about all possible inner commands)
;;
;; - How to share keybindings for the same activity everywhere it
;; occurs in the tree (i.e. using C-left always goes to beginning of
;; line no matter what the buffer looks like)
;;

(struct desc (doc args))
(struct arg-desc (doc id def))

;; A cmd is either a symbol or a list with a symbol as the first

(struct disp-cap ())
(struct dc:char-term disp-cap (rows cols))
;; XXX pixel-term with WxH?
;; XXX does it support various disp stuff

(struct disp ())
;; XXX a string
(struct d:row (ds))
(struct d:col (ds))
(define d:horiz d:col)
(define d:vert d:row)
;; XXX image
;; XXX has-focus
;; XXX web display
;; XXX colors

(define-generics app
  ;; app -> #f or app
  (app-delegate app)
  ;; app -> (listof symbol?)
  (app-commands app)
  ;; app symbol? -> desc?
  (app-command-desc app cmd-id)
  ;; app cmd -> app
  (app-exec app cmd)
  ;; app disp-cap -> disp
  ;; xxx take a context to show different in different places
  (app-render app disp-cap)
  ;; xxx to-update evt
  ;; xxx ready when it should be updated
  ;; app -> serializable
  (app-serialize app)
  #:fallbacks
  [(define (app-delegate a) #f)
   (define (app-commands a) empty)])

(provide (all-defined-out))
