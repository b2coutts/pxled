#lang racket
;; contains functions for drawing various UI pieces

(provide getcol draw-pixel draw-cursor draw-cmd draw-info draw-all)

(require "state.rkt")

(require racket/gui/base
         (except-in 2htdp/image make-color make-pen)
         (only-in mrlib/image-core render-image))

(define black (color 0 0 0 255))
(define white (color 255 255 255 255))
(define red (color 255 0 0 255))
(define green (color 0 255 0 255))
(define blue (color 0 0 255 255))

;; some constants for the UI
(define info/cmd-font-size 14)
(define info/cmd-height (image-height (text/font "l" info/cmd-font-size "white" "courier" 'modern
                                                 'normal 'normal #f)))
(define min-width 300) ;; make sure all usages fit in this width; maybe at least 80 characters?
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
    (text/font str info/cmd-font-size col "Courier" 'modern 'normal 'normal #f)))

(define/contract (getcol st x y)
  (-> state? integer? integer? color?)
  (define col (make-object color%))
  (send (state-bmp-dc st) get-pixel x y col)
  (color (send col red) (send col green) (send col blue)
         (* (inexact->exact (floor (send col alpha))) 255)))

;; TODO: deal with transparency, maybe by overlaying the solid rectangle over a checker pattern
(define/contract (draw-pixel st x y)
  (-> state? integer? integer? void?)
  (define col (getcol st x y))
  (define img (rectangle (state-zoom st) (state-zoom st) "solid" col))
  ;; TODO: off-by-one error with last 2 args to render-image?
  (render-image img (send (state-cvs st) get-dc)
                    (* x (state-zoom st))
                    (+ (* y (state-zoom st)) info/cmd-height)))

;; invert the given color
(define/contract (invert-color col)
  (-> color? color?)
  (match-define (color r g b a) col)
  (color (- 255 r) (- 255 g) (- 255 b) a))

;; draws the cursor
;; TODO: make this look nicer
(define/contract (draw-cursor st)
  (-> state? void?)
  (define-values (cvs zoom x y) (values (state-cvs st) (state-zoom st) (state-x st) (state-y st)))
  (define col (vector-ref (state-brushes st) (state-curbrush st)))
  (define empty-rec (rectangle zoom zoom "solid" (color 0 0 0 0)))
  (define radius (floor (* zoom 0.3)))
  (define cursor-img (cond
    [(not (hash-ref (state-cfg st) 'show-cursor)) empty-rec]
    [(< zoom 4) (rectangle zoom zoom "solid" col)]
    [(<= 4 zoom 9) (overlay (circle radius "solid" col) empty-rec)]
    [else (overlay (circle (- radius 1) "solid" col)
                   (circle radius "solid" (invert-color col))
                   empty-rec)]))
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
    [cmd (render-image (texttt st (string-append ":" cmd) cmd-fg) dc 1 ypos)]
    [err (render-image (texttt st err err-fg) dc 1 ypos)]
    ;; TODO: add a * for dirty file
    [else (render-image (texttt st (format "~a" (state-filename st)) fname-fg) dc 1 ypos)]))

;; produces the info string, to be drawn at the top of the string
;; TODO: colour it
(define/contract (get-info st)
  (-> state? string?)
  (match-define (state cvs width height zoom filename x y bmp-dc show-cursor? brushes curbrush
                       undos cmd err cfg) st)
  (match-define (color r g b a) (vector-ref brushes curbrush))
  (format "[~a,~a]   ~x~x~x (A=~x)" x y r g b a))

;; display a byte in two-digit hex notation
(define/contract (show-byte b)
  (-> byte? string?)
  (string-upcase (string-append (if (< b 16) "0" "") (format "~x" b))))

;; produces a list of pieces of the info string, with their colours
(define/contract (mk-infos st)
  (-> state? (listof (cons/c string? color?)))
  (match-define (state cvs width height zoom filename x y bmp-dc show-cursor? brushes curbrush
                       undos cmd err cfg) st)
  (match-define (color r g b a) (vector-ref brushes curbrush))
  (list (cons (format "(~a,~a)   #" x y) white)
        (cons (show-byte r) red)
        (cons (show-byte g) green)
        (cons (show-byte b) blue)
        (cons (format " (α=~a)" (show-byte a)) white)))

;; draws the info line at the top
(define/contract (draw-info st)
  (-> state? void?)
  (define (mktxt strcol) (text/font (car strcol) info/cmd-font-size (cdr strcol) "Courier" 'modern
                                    'normal 'normal #f))
  (define img (overlay/align "left" "top"
                (apply beside (map mktxt (mk-infos st)))
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
