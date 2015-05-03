#lang racket
;; contains functions for drawing various UI pieces

(provide draw-pixel draw-cursor draw-cmd draw-info draw-all)

(require "state.rkt")

(require racket/gui/base
         (except-in 2htdp/image make-color make-pen)
         (only-in mrlib/image-core render-image))

(define black (color 0 0 0 255))
(define white (color 255 255 255 255))
(define red (color 255 0 0 255))

;; some constants for the UI
(define min-width 300) ;; make sure all usages fit in this width; maybe at least 80 characters?
(define info/cmd-height 14)
(define cmd-bg black)
(define cmd-fg white)
(define err-fg red)
(define info-fg white)
(define fname-fg white)

;; a smallish monospaced text function
(define/contract (texttt st str col)
  (-> state? string? color? image?)
  (underlay/align "left" "top"
    (rectangle (send (state-cvs st) get-width) info/cmd-height "solid" cmd-bg)
    (text/font str info/cmd-height col "Courier" 'modern 'normal 'normal #f)))

(define/contract (getcol st x y)
  (-> state? integer? integer? color?)
  (define col (make-object color%))
  (send (state-bmp-dc st) get-pixel x y col)
  (color (send col red) (send col green) (send col blue) (* (floor (send col alpha)) 255)))

;; TODO: deal with transparency, maybe by overlaying the solid rectangle over a checker pattern
(define/contract (draw-pixel st x y)
  (-> state? integer? integer? void?)
  (define col (getcol st x y))
  (define img (rectangle (state-zoom st) (state-zoom st) "solid" col))
  ;; TODO: off-by-one error with last 2 args to render-image?
  (render-image img (send (state-cvs st) get-dc)
                    (* x (state-zoom st))
                    (+ (* y (state-zoom st)) info/cmd-height)))

;; draws the cursor
;; TODO: make this look nicer
(define/contract (draw-cursor st)
  (-> state? void?)
  (define-values (cvs zoom x y) (values (state-cvs st) (state-zoom st) (state-x st) (state-y st)))
  (define lzm (sub1 zoom))
  (define cursor-img (overlay (line lzm lzm "red") (line lzm (- lzm) "red")))
  (render-image cursor-img (send cvs get-dc) (* x zoom) (+ (* y zoom) info/cmd-height)))

;; function which gets the height in pixels of the image display area
(define/contract (img-disp-height st)
  (-> state? integer?)
  (* (state-height st) (state-zoom st)))

;; draws the cmd line. If the user is entering a command, displays their partial command. If not,
;; but there is an error message to display, displays the error message. Otherwise, displays the
;; filename
(define/contract (draw-cmd st)
  (-> state? void?)
  (define-values (err cmd) (values (state-err st) (state-cmd st)))
  (define dc (send (state-cvs st) get-dc))
  (define ypos (+ (img-disp-height st) info/cmd-height 1)) ;; TODO: off-by-one?
  (cond
    [cmd (render-image (texttt st cmd cmd-fg) dc 1 ypos)]
    [err (render-image (texttt st err err-fg) dc 1 ypos)]
    ;; TODO: add a * for dirty file
    [else (render-image (texttt st (format "~a" (state-filename st)) fname-fg) dc 1 ypos)]))

;; draws the info line at the top
(define/contract (draw-info st)
  (-> state? void?)
  (define img (overlay/align "left" "top"
                (texttt st (get-info st) white)
                (rectangle (send (state-cvs st) get-width) info/cmd-height "solid" black)))
  (render-image img (send (state-cvs st) get-dc) 0 0))

;; draws everything
(define/contract (draw-all st)
  (-> state? void?)
  (for ([x (state-width st)])
    (for ([y (state-height st)])
      (draw-pixel st x y)))
  (draw-cursor st)
  (draw-cmd st)
  (draw-info st))
