#lang racket

(require art)

(define-art-object (mxml-segment [notes]))

(define-art-rewriter load-musicxml
  (λ (stx)
    (syntax-parse stx
      [(_ file:string)
       ])))