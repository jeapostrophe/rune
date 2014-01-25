#lang racket/base
(require ffi/unsafe
         racket/class
         mred/private/wx/gtk/utils
         mred/private/wx/gtk/widget
         mred/private/wx/gtk/window
         mred/private/wx/gtk/client-window
         (prefix-in gui: racket/gui/base)
         mred/private/wx/gtk/panel
         mred/private/wx/gtk/types)

(provide
 (protect-out
  socket%))

(define-gtk gtk_socket_new (_fun -> _GtkWidget))
(define-gtk gtk_socket_add_id (_fun _GtkWidget _int -> _void))
(define-gtk gtk_socket_steal (_fun _GtkWidget _int -> _void))
(define-gtk gtk_socket_get_id (_fun _GtkWidget -> _int))

(define-gtk gtk_widget_size_allocate 
  (_fun _GtkWidget _GtkAllocation-pointer -> _void))

(define socket%
  (class gui:panel%
    (super-new)

    (define gtk (as-gtk-allocation (gtk_socket_new)))
    (gtk_widget_show gtk)
    (gtk_container_add (send this get-client-handle) gtk)

    (define filled? #f)
    (define (protect)
      (when filled?
        (error 'socket% "Can only be filled once."))
      (set! filled? #t))

    (define/public (get-id)
      (protect)
      (gtk_socket_get_id gtk))

    (define/public (add-id id)
      (protect)
      (gtk_socket_add_id gtk id))

    (define/public (steal-id id)
      (protect)
      (gtk_socket_steal gtk id))

    (define/override (on-size w h)
      (socket-set-size w h)
      (super on-size w h))

    (define (socket-set-size w h)
      (define child-gtk gtk)
      (define x 0)
      (define y 0)
      (gtk_widget_set_size_request child-gtk w h)
      (gtk_widget_size_allocate child-gtk (make-GtkAllocation x y w h)))))
