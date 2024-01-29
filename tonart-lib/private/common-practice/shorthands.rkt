#lang racket

(require art rsound "lib.rkt" (for-syntax syntax/parse))

(provide (all-from-out "lib.rkt") 
         (rename-out 
           [note n]
           [note->midi n->mid]))
