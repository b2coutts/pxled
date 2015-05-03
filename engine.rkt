#lang racket
;; the heart of pxled; handles key input, manages state, calls drawing functions

(provide handle-ke!)

(require "draw.rkt" "state.rkt" "cmd.rkt" "keys.rkt")

(require racket/gui/base)

(define/contract (handle-ke! st ke)
  (-> state? (is-a?/c key-event%) void?)
  (match-define (state cvs width height zoom filename x y bmp-dc show-cursor? brushes curbrush
                       undos cmd err cfg) st)
  (define code (send ke get-key-code))
  ;(printf "DEBUG: state is: ~s\n" st)
  (printf "DEBUG: code is: ~s\n" code)
  (printf "DEBUG: cmd is: ~s\n" cmd)
  (cond
    ;; TODO: implement some fancy features like arrow keys and ^W
    [cmd (match code
      ['escape (set-state-cmd! st #f)
               (draw-cmd st)]
      [(or 'return #\return) (set-state-err! st (exec-cmd! st cmd))
                             (set-state-cmd! st #f)
                             (draw-cmd st)]
      [(or 'backspace #\backspace)
        (define strlen (string-length cmd))
        (when (> strlen 0)
          (set-state-cmd! st (substring cmd 0 (- strlen 1)))
          (draw-cmd st))]
      [(? char?) (set-state-cmd! st (string-append cmd (~a code)))
                 (draw-cmd st)]
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
