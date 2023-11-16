#lang info

(define version "0.0.0")
(define collection "tonart")
(define deps '("base"))
(define build-deps '("scribble-lib"
                     "scribble-abbrevs"
                     "scribble-math"
                     "racket-doc"
                     "tonart-lib"))
(define scribblings '(("scribblings/tonart.scrbl" (multi-page) (language))))
(define clean '("compiled" "doc" "doc/tonart"))
