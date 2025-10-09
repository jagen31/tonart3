#lang racket

(require art tonart/base tonart/common-practice tonart/private/electronic/lib (for-syntax syntax/parse racket/dict))

(define-art-object (registration []))
(define-art-object (registration+ []))
(define-art-object (registration- []))

(define-art-object (manual []))
(define-art-object (manual-channel-mapping []))
(define-art-object (stop-data-config []))

(define-art-object (piston []))
(define-art-object (piston-data-config []))

(define-mapping-rewriter (registration->full-midi [(: reg registration)])
  (λ (stx reg)
    (syntax-parse stx
      [(_ channel:number)
         (define piston-config (require-context (lookup-ctxt) reg #'piston-data-config))
         (define cancel (run-art-exprs (list (qq-art reg (piston gc)) #'(piston->full-midi channel)) 
                                       '()
                                       (list piston-config)))
         (define/syntax-parse (_ stop-config ...) (require-context (lookup-ctxt) reg #'stop-data-config))
         (define stop-config* (syntax->datum #'(stop-config ...)))
         (syntax-parse reg
           [(_ {man stop ...} ...)
            (define results
            (for/fold ([acc '()])
                      ([man* (syntax->datum #'(man ...))]
                       [stops (syntax->datum #'((stop ...) ...))])
                (define my-stop-config (dict-ref stop-config* man*))
                (append
                    (for/list ([stop stops])
                        (define/syntax-parse data-num (car (dict-ref my-stop-config stop)))
                        (qq-art reg (full-midi data-num 127 () channel)))
                    acc)))
                #`(context #,@cancel #,@results)])])))

(define-mapping-rewriter (registration+->full-midi [(: reg registration+)])
  (λ (stx reg)
    (syntax-parse stx
      [(_ channel:number)
         (define piston-config (require-context (lookup-ctxt) reg #'piston-data-config))
         (define cancel (run-art-exprs (list (qq-art reg (piston gc)) #'(piston->full-midi channel)) 
                                       '()
                                       (list piston-config)))
         (define/syntax-parse (_ stop-config ...) (require-context (lookup-ctxt) reg #'stop-data-config))
         (define stop-config* (syntax->datum #'(stop-config ...)))
         (syntax-parse reg
           [(_ {man stop ...} ...)
            (define results
            (for/fold ([acc '()])
                      ([man* (syntax->datum #'(man ...))]
                       [stops (syntax->datum #'((stop ...) ...))])
                (define my-stop-config (dict-ref stop-config* man*))
                (append
                    (for/list ([stop stops])
                        (define/syntax-parse data-num (car (dict-ref my-stop-config stop)))
                        (qq-art reg (full-midi data-num 127 () channel)))
                    acc)))
                #`(context #,@results)])])))

(define-mapping-rewriter (piston->full-midi [(: p piston)])
  (λ (stx p)
    (syntax-parse stx
      [(_ channel:number)
       (define/syntax-parse (_ piston-config ...) (require-context (lookup-ctxt) p #'piston-data-config))
       (define/syntax-parse (_ p-name) p)
       (define/syntax-parse p-num (car (dict-ref (syntax->datum #'(piston-config ...)) (syntax-e #'p-name))))
       (qq-art p (full-midi p-num 127 () channel))])))

(define-mapping-rewriter (manual->channel [(: m manual)])
  (λ (stx m)
    (syntax-parse stx
      [(_)
       (define/syntax-parse (_ manual-config ...) (require-context (lookup-ctxt) m #'manual-channel-mapping))
       (define/syntax-parse (_ m-name) m)
       (define/syntax-parse m-num (car (dict-ref (syntax->datum #'(manual-config ...)) (syntax-e #'m-name))))
       (qq-art m (channel m-num))])))

(module+ test
(qr
    (manual-channel-mapping {I 1} {II 2} {III 3})

    (stop-data-config
    {I
        [flute-16 1]
        [montre 2]
        [harm-flute 3]}
    {II
        [cor-de-nuit 21]
        [salicional 22]
        [unda-maris 23]}
    {III 
        [flute 41]
        [bourdon 42]
        [aeoline 43]})

    (piston-data-config [gc 90])

    (registration {I flute-16}
                  {II cor-de-nuit}
                  {III flute})

    (registration->full-midi 8)))

(provide (all-defined-out))