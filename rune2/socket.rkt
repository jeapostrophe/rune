#lang racket/base
(require ffi/unsafe
         racket/class
         mred/private/wx/gtk/utils
         mred/private/wx/gtk/widget
         mred/private/wx/gtk/window
         mred/private/wx/gtk/client-window
         mred/private/wx/gtk/panel
         mred/private/wx/gtk/types)

(provide
 (protect-out
  socket%))

(define-gtk gtk_socket_new (_fun -> _GtkWidget))
(define-gtk gtk_socket_add_id (_fun _GtkWidget _int -> _void))
(define-gtk gtk_socket_steal (_fun _GtkWidget _int -> _void))
(define-gtk gtk_socket_get_id (_fun _GtkWidget -> _int))

(define-gtk gtk_widget_size_allocate (_fun _GtkWidget _GtkAllocation-pointer -> _void))
(define-gdk gdk_get_default_root_window (_fun -> _GdkWindow))
(define-gdk gdk_window_get_parent (_fun _GdkWindow -> _GdkWindow))
(define-gdk gdk_window_foreign_new (_fun _int -> _GdkWindow))
(define-gdk gdk_window_reparent (_fun _GdkWindow _GdkWindow _int _int -> _void))

(define socket%
  (class* object% ()
    (init parent)

    (define gtk (as-gtk-allocation (gtk_socket_new)))
    (gtk_widget_show gtk)

    (gtk_container_add (send parent get-client-handle) gtk)

    (define/public (get-id)
      (gtk_socket_get_id gtk))

    (define embedded-id #f)
    (define (reparent-old)
      (when embedded-id
        (printf "reparent ~a to ~a\n" embedded-id 'root)
        (define fw (gdk_window_foreign_new embedded-id))
        (gdk_window_reparent fw (gdk_get_default_root_window) 0 0)
        (set! embedded-id #f)))
    (define (new-embedding id)
      (set! embedded-id id)
      (set-last-size))

    (define/public (add-id id)
      (reparent-old)
      (gtk_socket_add_id gtk id)
      (new-embedding id))

    (define/public (steal-id id)
      (reparent-old)
      (gtk_socket_steal gtk id)
      (new-embedding id))

    (define last-w #f)
    (define last-h #f)

    (define (set-last-size)
      (when (and last-w last-h)
        (really-set-size 0 0)
        (really-set-size last-w last-h)))

    (define/public (set-size w h)
      (set! last-w w)
      (set! last-h h)
      (really-set-size w h))

    (define (really-set-size w h)
      (define child-gtk gtk)
      (define x 0)
      (define y 0)
      (gtk_widget_set_size_request child-gtk w h)
      (gtk_widget_size_allocate child-gtk (make-GtkAllocation x y w h)))

    (super-new)))
