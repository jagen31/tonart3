#lang racket

(require "lib.rkt" tonart (for-syntax racket/base syntax/parse racket/string racket/runtime-path))

(define-syntax (#%bodule-megin stx)
  (syntax-parse stx
    [(_ ({~datum filename} name:string) ({~datum voices} [voices ...]))
     (define dir (current-load-relative-directory))
     (define the-score
       (run-art-exprs
        (list #`(load-musicxml #,(path->string (build-path dir (syntax-e #'name))) [voices ...])
              #'(musicxml->tonart))
        '()))

    (define-values (the-score* the-header)
       (for/fold ([the-score* '()] [the-header '()] #:result (values (reverse the-score*) (reverse the-header)))
                 ([expr the-score])

         (syntax-parse expr
           [({~literal direction} dir)
            (define dat (read (open-input-string (string-replace (syntax-e #'dir) "&quot;" "\""))))
            (define/syntax-parse result (datum->syntax expr dat))
            (cond [(not (list? dat)) (values the-score* the-header)]
                  [else (values (cons (qq-art expr result) the-score*) the-header)])]
           [({~literal credit} text)
            #:with result (datum->syntax expr (read (open-input-string (string-replace (syntax-e #'text) "&quot;" "\""))))
            (values the-score (cons #'result the-header))]
           [_ (values (cons expr the-score*) the-header)])))

     #`(#%module-begin 
        #,@the-header
        #;(define-art the-score #,@(map un-@ the-score*) (do-set)  )
        #;(provide the-score)
        (realize (namespace-define-realizer) 
                 #,@(map un-@ the-score*) (do-set) (instant->interval) (apply-coordinates)  
                 (split-on-reset) (resolve-ref*)))]))


(provide read read-syntax #%top #%app #%datum (rename-out [#%bodule-megin #%module-begin])
         (except-out (all-from-out racket) #%module-begin) (all-from-out tonart))