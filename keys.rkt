#lang racket
;; file containing the key mappings

(provide bindings)

(require "state.rkt")

(require racket/gui/base)

(define/contract (bindings st ke)
  (-> state? (is-a?/c key-event%) (or/c string? #f))
  (hash/c char? string?)
  (match-define (state cvs width height zoom filename x y bmp-dc show-cursor? brushes curbrush
                       undos cmd err misc) st)
  (define col (vector-ref brushes curbrush))
  (define code (send ke get-key-code))
  (define ctrl (send ke get-control-down))
  (match (list code ctrl)
    [`(#\h #f) "move -1 0"]
    [`(#\H ,_) "move -5 0"]
    [`(#\h #t) "move -10 0"]
    [`(#\j #f) "move 0 1"]
    [`(#\J ,_) "move 0 5"]
    [`(#\j #t) "move 0 10"]
    [`(#\k #f) "move 0 -1"]
    [`(#\K ,_) "move 0 -5"]
    [`(#\k #t) "move 0 -10"]
    [`(#\l #f) "move 1 0"]
    [`(#\L ,_) "move 5 0"]
    [`(#\l #t) "move 10 0"]
    [`(#\z #f) "zoom in"]
    [`(#\Z #f) "zoom out"]
    [`(#\d ,_) "draw"]
    [`(#\D ,_) "flood"]
    [`(#\c #f) "toggle-cursor"]
    [`(#\u #f) "undo"]
    [(list (or #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) _) (format "brush ~a" code)]
    ;[`(#\s ,_) "save"]
    [_ #f]))
