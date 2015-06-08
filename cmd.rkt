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

;; converts a color% to a color?
(define/contract (ccrev col)
  (-> (is-a?/c color%) color?)
  (color (send col red) (send col green) (send col blue)
         (* (inexact->exact (floor (send col alpha))) 255)))

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
  (draw-cursor st)
  (draw-cmd st)
  (draw-info st))

;; flood-fills fcol over the surrounding area, while the color it's covering is ccol
;; returns a list of positions for undos
(define/contract (flood-fill! st fcol ccol x y)
  (-> state? color? color? integer? integer? (listof (cons/c integer? integer?)))
  (cond
    [(and (<= 0 x (state-width st))
          (<= 0 y (state-height st)))
      (define pxcol (getcol st x y))
      (cond
        [(equal? fcol ccol) (error "ERROR: flood-fill! given equal fcol and ccol!")]
        [(equal? pxcol ccol)
          (send (state-bmp-dc st) set-pixel x y (cc fcol))
          (draw-pixel st x y)
          (draw-cursor st)
          (append
            (list (cons x y))
            (flood-fill! st fcol ccol (add1 x) y)
            (flood-fill! st fcol ccol (sub1 x) y)
            (flood-fill! st fcol ccol x (add1 y))
            (flood-fill! st fcol ccol x (sub1 y)))]
        [else '()])]
    [else '()]))

;; save the file
(define/contract (save-img st filename)
  (-> state? path-string? void?)
  (send (send (state-bmp-dc st) get-bitmap) save-file filename 'png)
  (hash-set! (state-misc st) 'dirty #f))

;; loads the given file
(define/contract (load-img! st fname)
  (-> state? path-string? void?)
  (define bmp (make-object bitmap% 1 1 #f #t 1.0))
  (send bmp load-file fname 'unknown #f #t)
  (send (state-bmp-dc st) set-bitmap bmp)
  (set-state-filename! st fname)
  (set-state-width! st (send bmp get-width))
  (set-state-height! st (send bmp get-height))
  (set-state-x! st 0)
  (set-state-y! st 0)
  (set-state-err! st #f)
  (hash-set! (state-misc st) 'dirty #f)
  (draw-all st))


;; change the zoom level
(define/contract (zoom! st lvl)
  (-> state? integer? void?)
  (set-state-zoom! st lvl)
  (send (send (state-cvs st) get-dc) clear)
  (draw-all st))

;; provides a usage string for the given command, produces false if no such command exists
(define/contract (get-usage cmd)
  (-> any/c string?)
  (define in (curry member cmd))
  (define msg
    (cond
      [(in '(amove amv)) "(x : int) (y : int). Move cursor to (x,y)."]
      [(in '(move mv)) "(x : int) (y : int). Move cursor to (x,y)."]
      [(in '(draw dr)) "[x : int] [y : int] [col : color]. Change color at (x,y) to col."]
      [(in '(fl flood)) "[x : int] [y : int] [col : color]. Flood with col, starting at (x,y)."]
      [(in '(save sv w))  "[fname : string]. Save the current image to fname."]
      [(in '(edit ed e)) "[fname : string]. Edit the file fname."]
      [(in '(edit! ed! e!)) "(fname : string). Discard changes to edit a file."]
      [(in '(zoom zo)) "(z : int). Display pixels with a box z px wide."]
      [(in '(r re red))  "(b : byte). Set the red color component to b."]
      [(in '(b bl blu blue))  "(b : byte). Set the blue color component to b."]
      [(in '(g gr gre gree green))  "(b : byte). Set the green color component to b."]
      [(in '(a al alp alph alpha))  "(b : byte). Set the alpha component to b."]
      [(in '(co color)) "(col : color). Set the color of the current brush."]
      [(in '(tc toggle-cursor)) "[v : boolean]. Toggle visibility of the cursor."]
      [(in '(un undo)) "[n : int]. Undo last n actions."]
      [else #f]))
  (if msg (format "Usage: ~a ~a" cmd msg)
          (format "Command not found: ~a" cmd)))

;; converts a hex string to a number
(define hex->num (curryr string->number 16))

;; converts an ast to a color, or #f if the ast is not a valid color
(define/contract (ast->color ast)
  (-> any/c (or/c color? #f))
  (define str (cond
    [(string? ast) ast]
    [(symbol? ast) (symbol->string ast)]
    [else ""]))
  (cond
    [(send the-color-database find-color str) (ccrev (send the-color-database find-color str))]
    [else (match (string-length str)
      [6 (match (list (substring str 0 2) (substring str 2 4) (substring str 4 6))
        [(list (app hex->num (? integer? r)) (app hex->num (? integer? g))
               (app hex->num (? integer? b)))
          (color r g b 255)]
        [_ #f])]
      [8 (match (list (substring str 0 2) (substring str 2 4)
                      (substring str 4 6) (substring str 6 8))
        [(list (app hex->num (? integer? r)) (app hex->num (? integer? g))
               (app hex->num (? integer? b)) (app hex->num (? integer? a)))
          (color r g b a)]
        [_ #f])]
      [_ #f])]))

;; undoes the last n actions
(define/contract (undo! st n)
  (-> state? exact-nonnegative-integer? void?)
  (when (> n 0)
    (match (state-undos st)
      ['() (void)]
      [(cons undo rst)
        (define-values (x y) (match undo
          [(list 'single x y col) (send (state-bmp-dc st) set-pixel x y (cc col))
                                  (draw-pixel st x y)
                                  (values x y)]
          [(list 'flood-fill xs ys col) (for ([x xs] [y ys])
                                          (send (state-bmp-dc st) set-pixel x y (cc col))
                                          (draw-pixel st x y))
                                        (values (first xs) (first ys))]))
        (set-state-undos! st rst)
        (mv-cursor! st x y)
        (undo! st (- n 1))])))

;; attempts to parse a string as an integer or a color; returns the string itself on failure
(define/contract (try-parse str)
  (-> string? (or/c string? color? integer?))
  (match str
    [(regexp "#(..)(..)(..)" (list _ (app hex->num (? integer? r))
                                     (app hex->num (? integer? g))
                                     (app hex->num (? integer? b))))
      (color r g b 255)]
    [(regexp "0x(..)(..)(..)(..)"
        (list _ (app hex->num (? integer? r)) (app hex->num (? integer? g))
                (app hex->num (? integer? b)) (app hex->num (? integer? a))))
      (color r g b a)]
    [(app string->number (? integer? i)) i]
    [x x]))

;; executes a piece of user code
;; TODO: maybe sandbox their code somehow?
(define/contract (exec-user-code code)
  (-> string? (or/c (cons 'fail string?) (cons 'success any/c)))
  (define ast
    (with-handlers ([exn:fail:read?
                      (lambda (e) (cons 'fail (format "Parse error: ~a" (exn-message e))))])
      (with-input-from-string code (thunk (read)))))
  (match ast
    [(cons 'fail _) ast]
    [_ (with-handlers ([exn:fail? (lambda (e) (cons 'fail (format "Execution failed: ~a"
                                                                  (exn-message e))))])
        (cons 'success (eval ast)))]))

;; attempts to execute a user command; returns #f on success, or an error string on error
(define/contract (exec-cmd! st cmdstr)
  (-> state? string? (or/c string? #f))
  (printf "DEBUG: exec-cmd!: cmdstr is: ~s\n" cmdstr)
  (match-define (state cvs width height zoom filename x y bmp-dc show-cursor? brushes curbrush
                       undos scmd err misc) st)
  (define brush-col (vector-ref brushes curbrush))
  (define words (map try-parse (string-split cmdstr " ")))
  (define cmd (if (empty? words) #f (first words)))
  (with-handlers () ;; placeholder for handling user draw errors
    (define usg (if cmd (get-usage cmd) #f))
    (match words
      ;; absolute move
      [(list (or "amove" "amv") (? integer? a) (? integer? b))
        (mv-cursor! st a b) #f]
      [(cons (or "amove" "amv") _) usg]

      ;; relative move
      [(list (or "move" "mv") (? integer? a) (? integer? b))
        (mv-cursor! st (+ x a) (+ y b)) #f]
      [(cons (or "move" "mv") _) usg]

      ;; draw
      [(cons (or "draw" "dr" "flood" "fl") args)
        (define-values (a b col) (match args
          ['() (values x y brush-col)]
          [(list (? integer? a)) (values a y brush-col)]
          [(list (? integer? a) (? integer? b)) (values a b brush-col)]
          [(list (? integer? a) (? integer? b) (? color? col)) (values a b col)]
          [_ (values #f #f #f)]))

        (define ccol (if a (getcol st a b) (void)))
        (cond
          [(not a) usg]
          ;; single point draw
          [(member cmd '("draw" "dr"))
            (set-state-undos! st (cons (list 'single a b ccol) undos))
            (send bmp-dc set-pixel a b (cc col))
            (hash-set! misc 'dirty #t)
            (draw-pixel st a b)
            (draw-cursor st)
            (draw-cmd st)
            #f]
          ;; flood-fill
          [(equal? ccol col) #f]
          [else (define xys (flood-fill! st col ccol a b))
                (hash-set! misc 'dirty #t)
                (set-state-undos! st (cons (list 'flood-fill (map car xys) (map cdr xys) ccol)
                                           undos))
                (draw-cmd st)
                #f])]
      
      ;; save
      [(cons (or "save" "sv" "w") args) (match args
        ['() (cond
          [filename (save-img st filename) #f]
          [else "You must specify a filename"])]
        [(list (? string? fname))
          (unless filename
            (set-state-filename! st fname))
          (save-img st fname)
          #f]
        [_ usg])]
      
      ;; load/edit
      [(cons (or "edit" "ed" "e" "edit!" "ed!" "e!") args) (cond
        [(and (hash-ref misc 'dirty) (member cmd '("edit" "ed" "e"))) (cond
          [filename (format "~a has unsaved changes. Use ~a! to discard." filename cmd)]
          [else (format "Your file is not saved. Use ~a! to discard." cmd)])]
        [else (match args
          ['() (cond
            [filename (load-img! st filename) #f]
            [else "You must specify a filename to edit."])]
          [(list (? string? fname)) (load-img! st fname)
                                    #f]
          [_ usg])])]

      ;; zoom
      [(cons (or "zoom" "zo") args) (match args
        ['("in") (zoom! st (+ zoom 1)) #f]
        ['("out") (when (> zoom 1)
                  (zoom! st (- zoom 1)))
                #f]
        [(list (? exact-positive-integer? i)) (zoom! st i) #f]
        [_ usg])]

      ;; adjust individual colours
      [(cons (and (or "r" "re" "red" "b" "bl" "blu" "blue" "g" "gr" "gre" "gree" "green"
                      "a" "al" "alp" "alph" "alpha") col) args) (match args
        [(list (? byte? byte))
          (match-define (color r g b a) brush-col)
          (vector-set! brushes curbrush (color
            (if (member col '("r" "re" "red")) byte r)
            (if (member col '("g" "gr" "gre" "gree" "green")) byte g)
            (if (member col '("b" "bl" "blu" "blue")) byte b)
            (if (member col '("a" "al" "alp" "alph" "alpha")) byte a)))
          (draw-info st)
          (draw-cursor st)
          #f]
        [else usg])]

      ;; adjust all colours, simultaneously
      [(cons (or "co" "color") args) (match args
        [(list (? color? col))
          (vector-set! brushes curbrush col)
          (draw-info st)
          (draw-cursor st)
          #f]
        [_ usg])]

      ;; set cursor visibility
      [(cons (or "tc" "toggle-cursor") args) (match args
        ['() (hash-set! misc 'show-cursor (not (hash-ref misc 'show-cursor)))
             (draw-pixel st x y)
             (draw-cursor st)
             #f]
        [(list (? boolean? bool)) (hash-set! misc 'show-cursor bool)
                                  (draw-pixel st x y)
                                  (draw-cursor st)
                                  #f]
        [_ usg])]

      ;; undo last draw action
      [(cons (or "un" "undo") args) (match args
        ['() (undo! st 1) #f]
        [(list (? exact-nonnegative-integer? n)) (undo! st n) #f]
        [_ usg])]

      ;; command not found
      [(cons nm _) (format "Command not found: ~a" nm)]
      [_ (format "Invalid command: ~a" cmdstr)])))
