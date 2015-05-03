#lang racket
;; the heart of pxled; handles key input, manages state, calls drawing functions

(provide handle-ke!)

(require "draw.rkt" "state.rkt" "cmd.rkt" "keys.rkt")

(require racket/gui/base)

(define/contract (handle-ke! st ke)
  (-> state? (is-a?/c key-event%) void?)
  (match-define (state cvs width height zoom filename x y bmp-dc show-cursor? brushes curbrush
                       undos cmd err misc) st)
  (define code (send ke get-key-code))
  (define ctrl (send ke get-control-down))
  ;(printf "DEBUG: code is: ~s\n" code)
  (cond
    ;; TODO: allow the user to move the cursor
    [cmd (match code
      ['escape (set-state-cmd! st #f)
               (draw-cmd st)]
      [(or 'return #\return) (set-state-err! st (exec-cmd! st cmd))
                             (set-state-cmd! st #f)
                             (draw-cmd st)]
      [(or 'backspace #\backspace)
        (define strlen (string-length cmd))
        (cond
          [(> strlen 0) (set-state-cmd! st (substring cmd 0 (- strlen 1)))
                        (draw-cmd st)]
          [else (set-state-cmd! st #f)
                (draw-cmd st)])]
      [(? char?) (cond
        [ctrl (match code
          [#\w (set-state-cmd! st (string-join (drop-right (string-split cmd " ") 1)))]
          [#\u (set-state-cmd! st "")]
          [#\c (set-state-cmd! st #f)]
          [_ (void)])
          (draw-cmd st)]
        [else (set-state-cmd! st (string-append cmd (~a code)))
              (draw-cmd st)])]
      [_ (void)])]
    [(equal? code 'escape) (set-state-err! st #f)
                           (draw-cmd st)]
    [(equal? code #\:) (set-state-cmd! st "")
                       (set-state-err! st #f)
                       (draw-cmd st)]
    [else (define key-cmd (bindings st ke))
          (when key-cmd
            (define result (exec-cmd! st key-cmd))
            (when result
              (set-state-err! st result)
              (printf "DEBUG: setting err to: ~s\n" result)
              (draw-cmd)))]))
