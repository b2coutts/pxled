#lang racket

(require "engine.rkt" "draw.rkt" "state.rkt" "cmd.rkt")

(require racket/gui/base
         (except-in 2htdp/image make-color make-pen)
         (only-in mrlib/image-core render-image))

(define st (void))

(define width (make-parameter 10))
(define height (make-parameter 10))
(define zoom (make-parameter 16))

(define filename (command-line
  #:program "pxled"
  #:once-each
  [("-x" "--width") w
                    "Edit an image w pixels wide"
                    (width (string->number w))]
  [("-y" "--height") h
                     "Edit an image h pixels high"
                     (height (string->number h))]
  [("-z" "--zoom") z
                   "Display each pixel with a square z px wide"
                   (zoom (string->number z))]

  #:args ([filename #f])
  filename))


(define frm (new (class frame%
                  (super-new)
                  (define/augment (on-close)
                    (custodian-shutdown-all (current-custodian))))
                 [label "pxled"])) ;; TODO: nice label with filename
(define cvs (new (class canvas%
                  (super-new)
                  (define/override (on-char ke)
                    (handle-ke! st ke)))
                 [parent frm]
                 [paint-callback (lambda (cvs dc) (draw-all st))]))

(define bgcolor (make-object color% 30 30 30 1.0))
                                  
(send cvs min-client-width (width))
(send cvs min-client-height (height))
(send cvs set-canvas-background bgcolor)
(send (send cvs get-dc) set-background bgcolor)
(send frm show #t)
(send cvs focus)

(define bmp (make-object bitmap% (width) (height) #f #t))
(define bmp-dc (new bitmap-dc% [bitmap bmp]))
(for ([x (width)])
  (for ([y (height)])
    (send bmp-dc set-pixel x y (make-object color% 255 255 255 1.0))))

;; TODO: delete this block
(define tmpcol (make-object color%))
(send bmp-dc get-pixel 2 2 tmpcol)
(define (conv col) (color (send col red) (send col green) (send col blue) (send col alpha)))
(printf "bmp-dc at (2,2) is ~a\n" (conv tmpcol))

(define misc (make-hash `(
  [show-cursor . #t]
  [dirty . ,(if filename #f #t)]
)))

;; TODO: unhardcode this stuff
(set! st (state
  cvs
  (width)
  (height)
  (zoom)
  filename
  0
  0
  bmp-dc
  #t
  (make-vector 10 (color 0 0 0 255))
  1
  '()
  #f
  #f
  misc
))

(when filename
  (exec-cmd! st "edit"))

(send cvs on-paint)
(printf "pxled started.\n")
