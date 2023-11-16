#lang racket

(require art (for-syntax syntax/parse))
(provide (all-defined-out))

;; define-coordinate interval (copy-coordinate std:interval) or something
;; define-coordinate voice (copy-coordinate std:subset) or something

(define-art-object (tempo [bpm]))
(define-art-object (instrument [name]))

(define-art-rewriter musi@
  (λ (stx)
    (syntax-parse stx
      [(_ [start*:number end*:number (voice:id ...)] expr ...)
       (qq-art stx (@ [(interval (start start*) (end end*)) (subset voice ...)] expr ...))])))
