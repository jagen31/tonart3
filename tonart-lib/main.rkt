#lang racket

(require art tonart/base tonart/common-practice 
         tonart/rsound tonart/linuxsampler tonart/chuck tonart/private/church/hymn
         tonart/private/musicxml/lib xml
         (for-syntax syntax/parse))
(provide (all-from-out art tonart/base tonart/common-practice 
                       tonart/rsound tonart/linuxsampler tonart/chuck tonart/private/church/hymn
                       tonart/private/musicxml/lib xml))

(define-syntax dmr
  (syntax-parser
    [(_ [w:number h:number] expr ...) #'(dr [w h] (music expr ...))]
    [(_ expr ...) #'(dr (music expr ...))]))

(provide dmr)