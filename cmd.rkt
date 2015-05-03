#lang racket
;; module which implements the user commands

(provide exec-cmd!)

(require "state.rkt" "draw.rkt")

(require racket/gui/base
         (except-in 2htdp/image make-color make-pen)
         (only-in mrlib/image-core render-image))

;; a struct that can represent most command handlers
(define-struct/contract ch (
  [names (listof string?)] ;; list of words that can be used for the command
  [aparse (listof (-> string? (or/c (cons/c #t any/c) #f)))] ;; list of argument-parsers
  [min-args integer?] ;; minimum number of arguments needed
  [usage string?] ;; the usage string for this command
  [fn procedure?] ;; takes the state and above arguments and produces #f or an error string
))

;; converts a color? to a color%
(define/contract (cc col)
  (-> color? (is-a?/c color%))
  (match-define (color r g b a) col)
  (make-object color% r g b (/ a 255)))

;; parses a color from an AST, or returns #f for an invalid AST
(define/contract (ast->color ast)
  (-> any/c (or/c color? #f))
  (match ast
    [(list 'color (? byte? r) (? byte? g) (? byte? b) (? byte? a)) (color r g b a)]
    [(list 'color (? byte? r) (? byte? g) (? byte? b)) (color r g b 255)]
    [_ #f]))

;; moves the cursor to the given spot, snapping it to the canvas if needed
(define/contract (mv-cursor! st x y)
  (-> state? integer? integer? void?)
  (define (snap-int i low high)
    (cond
      [(< i low) low]
      [(> i high) high]
      [else i]))
  (draw-pixel st (state-x st) (state-y st))
  (set-state-x! st (snap-int x 0 (- (state-width st) 1)))
  (set-state-y! st (snap-int y 0 (- (state-height st) 1)))
  (draw-cursor st))

;; flood-fills fcol over the surrounding area, while the color it's covering is ccol
(define/contract (flood-fill! st fcol ccol x y)
  (-> state? color? color? integer? integer? void?)
  (when (equal? fcol ccol)
    (error "ERROR: flood-fill! given equal fcol and ccol!"))
  (define pxcol (make-object color%))
  (send (state-bmp-dc st) get-pixel x y pxcol)
  (when (and (equal? pxcol ccol)
             (<= 0 x (state-width st))
             (<= 0 y (state-height st)))
    (send (state-bmp-dc st) set-pixel x y (cc fcol))
    (draw-pixel st x y)
    (draw-cursor st)
    (flood-fill! st fcol ccol (add1 x) y)
    (flood-fill! st fcol ccol (sub1 x) y)
    (flood-fill! st fcol ccol x (add1 y))
    (flood-fill! st fcol ccol x (sub1 y))))

;; save the file; return #f on success, or an error string on failure
(define/contract (save-img st filename)
  (-> state? path-string? (or/c string? #f))
  "TODO: implement saving")

;; attempts to execute a user command; returns #f on success, or an error string on error
(define/contract (exec-cmd! st cmdstr)
  (-> state? string? (or/c string? #f))
  (printf "DEBUG: exec-cmd!: cmdstr is: ~s\n" cmdstr)
  (match-define (state cvs width height zoom filename x y bmp-dc show-cursor? brushes curbrush
                       undos cmd err cfg) st)
  (define fixstr (cond
    [(or (< (string-length cmdstr) 1) (equal? (substring cmdstr 0 1) "(")) cmdstr]
    [else (string-append "(" cmdstr ")")]))
  (with-handlers ([exn:fail:read? (lambda (e) "Syntax error.")])
    (define ast (with-input-from-string fixstr (thunk (read))))
    (match ast
      ;; absolute move
      [(list (or 'amove 'amv) (? integer? a) (? integer? b))
        (mv-cursor! st a b) #f]
      [(cons (or 'amove 'amv) _)
        "Usage: amove (x : int) (y : int). Move cursor to (x,y)."]

      ;; relative move
      [(list (or 'move 'mv) (? integer? a) (? integer? b))
        (mv-cursor! st (+ x a) (+ y b)) #f]
      [(cons (or 'move 'mv) _)
        "Usage: move (x : int) (y : int). Move cursor to (x,y)."]

      ;; draw
      [(cons (or 'draw 'dr 'flood 'fl) args)
        (define-values (a b col) (match args
          ['() (values x y (vector-ref brushes curbrush))]
          [(list (? integer? a)) (values a y (vector-ref brushes curbrush))]
          [(list (? integer? a) (? integer? b)) (values a b (vector-ref brushes curbrush))]
          [(list (? integer? a) (? integer? b) (app ast->color (? color? col))) (values a b col)]
          [_ (values #f #f #f)]))

        (define ccol (make-object color%))
        (send bmp-dc get-pixel a b ccol)
        (cond
          [(not a) "Usage: draw [x : int] [y : int] [col : color]. Change color at (x,y) to col."]
          ;; single point draw
          [(member (first ast) '(draw dr)) (send bmp-dc set-pixel a b (cc col))
                                           (draw-pixel st a b)
                                           (draw-cursor st)
                                           #f]
          ;; flood-fill
          [(equal? ccol col) #f]
          [else (flood-fill! st col ccol a b) #f])]
      
      ;; save
      [(cons (or 'save 'sv 'w) args) (match args
        ['() (save-img st filename)]
        [(list (? string? fname)) (save-img st fname)]
        [_ "Usage: save [fname : string]. Save the current image to fname."])]
      
      ;; command not found
      [(list nm) (format "Command not found: ~a" nm)]
      [_ (format "Invalid command: ~a" ast)])))
