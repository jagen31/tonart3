#lang racket

(require art rsound "lib.rkt" (for-syntax syntax/parse))

(provide (all-from-out "lib.rkt") 
         (rename-out [display-device-table ddt]
                     [set-output-device! sod!]))

(define-syntax mrsr
  (syntax-parser
    [(_ body ...) (syntax/loc this-syntax (realize (music-rsound-realizer) body ...))]))

(define-syntax pmrsr
  (syntax-parser
    [(_ body ...) (syntax/loc this-syntax (play (mrsr body ...)))]))

(provide mrsr pmrsr)