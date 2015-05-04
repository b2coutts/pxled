#lang racket

(provide (struct-out state) undo? state-filename-str)

(require racket/gui/base
         (except-in 2htdp/image make-color make-pen)
         (only-in mrlib/image-core render-image))

;; an undo action. either a flood-fill action, with a list of affected pixels, and an old color to
;; restore, otherwise a single action, with the affected pixel and old color
(define undo? (or/c (list/c 'flood-fill (listof integer?) (listof integer?) color?)
                    (list/c 'single integer? integer? color?)))

;; the struct which holds the state of pxled
(define-struct/contract state (
  [cvs (is-a?/c canvas%)] ;; canvas of the window
  [width integer?] ;; width in pixels of the image
  [height integer?] ;; height in pixels of the image
  [zoom integer?] ;; displayed width of an image pixel
  [filename (or/c path-string? #f)] ;; filename of image being edited
  [x integer?] ;; position of cursor
  [y integer?]
  [bmp-dc (is-a?/c bitmap-dc%)] ;; bitmap drawing context
  [show-cursor? boolean?] ;; toggles cursor visibility
  [brushes (vectorof color?)] ;; vector of brush colors
  [curbrush integer?] ;; index into above vector
  [undos (listof undo?)] ;; stack of undo actions to be done
  [cmd (or/c string? #f)] ;; user command handler
  [err (or/c string? #f)] ;; error message for user
  [misc hash?] ;; various misc. options
) #:mutable #:transparent)

;; get a readable string for the filename
(define/contract (state-filename-str st)
  (-> state? path-string?)
  (match (state-filename st)
    [(? string? s) s]
    [#f "[No Name]"]))
