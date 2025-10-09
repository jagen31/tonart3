#lang racket

(require tonart "lib.rkt" (for-syntax syntax/parse))

(define-art score
  (load-musicxml "test.musicxml" [one])
  (musicxml->tonart)
  (interpret-directions))