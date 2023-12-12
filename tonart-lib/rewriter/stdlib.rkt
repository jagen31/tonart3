#lang racket

(require art art/timeline/lib art/sequence/lib
         (for-syntax syntax/parse racket/list racket/match
                     tonart/rewriter/common-practice/tonality))
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



;; FIXME jagen SOMEWHAT SLOW (also somewhat broken)
(define-art-rewriter apply-tempo
  (λ (stx)
    (define-values (tempos* rest-ctxt)
      (partition (λ (x) (and (free-identifier=? (car (syntax->list x)) #'tempo)
                             (context-within? (get-id-ctxt x) (get-id-ctxt stx) (current-ctxt))))
                 (current-ctxt)))
    ;; This sort is because the previous tempos affect the later tempos.  we can avoid this
    ;; by beginning with the later tempos.
    (define tempos 
      (sort tempos* > 
        #:key (λ (x) (syntax-parse (context-ref (get-id-ctxt x) #'interval) 
                       [(_ (_ start:number) _) (syntax-e #'start)]))))
    (define new-ctxt 
    (for/fold ([acc rest-ctxt]) 
              ([tem tempos])
      (define iv (context-ref (get-id-ctxt tem) #'interval))
      ;; FIXME jagen yuck
      (syntax-parse #`(#,tem #,(or iv #'(interval (start 0) (end +inf.0))))
        [((_ tem*:number) (_ ({~literal start} s) ({~literal end} e)))
         (define tem** (syntax-e #'tem*))
         (define s* (syntax-e #'s))
         (define e* (syntax-e #'e))
         (for/list ([expr acc])
           (define iv* (context-ref (get-id-ctxt expr) #'interval))
           (cond [iv*
                  (syntax-parse iv*
                    [(_ ({~literal start} expr-s) ({~literal end} expr-e))
                     (match-define (list s** e**) 
                       (normalize-tempo-interval (list s* e*) tem** 
                         (list (syntax-e #'expr-s) (syntax-e #'expr-e))))
                     (put-in-id-ctxt expr #`(interval (start #,s**) (end #,e**)))])]
                 [else expr]))])))
    #`(@ () #,@(map delete-expr tempos) #,@(map delete-expr rest-ctxt) (put #,@new-ctxt))))

(define-art-embedding (music [items])
  (λ (stx ctxt)
    (syntax-parse stx
      [(head:id expr ...)
       (compile-rewrite-exprs (list (quasisyntax/loc stx (@ () expr ...))) '())])))

(define-mapping-rewriter (rewrite-in-music [(: s music)])
  (λ (stx s)
    (syntax-parse stx
      [(_ exprs ...)
       (syntax-parse s
         [(_ exprs* ...)
           #:with (result ...) 
             (compile-rewrite-exprs 
               (syntax->list #'(exprs ...)) (syntax->list #'(exprs* ...)))
           #`(@ () #,(delete-expr s) #,(qq-art s (music result ...)))])])))

(define-for-syntax (music-start stx)
  (syntax-parse stx
    [({~literal music} expr ...)
     (min (map expr-interval-start (syntax->list #'(expr ...))))]))

(define-for-syntax (music-end stx)
  (syntax-parse stx
    [({~literal music} expr ...)
     (apply max (map expr-interval-end (syntax->list #'(expr ...))))]))

(define-mapping-rewriter (inline-music-seq [(: s seq)])
  (λ (stx s)
    (define init-start (expr-interval-start s))
    (define init-end (expr-interval-end s))
    (println "HEREHEREHER")
    (syntax-parse s 
      [(_ expr ...)
        #:do [(define sorted (sort (syntax->list #'(expr ...)) < #:key expr-index))]
        #:with (result ...) 
          (for/list ([e sorted]) 
            (syntax-parse e
              [({~literal music} music-expr ...)
               #:with end (music-end e)
               #`[end #,@(for/list ([e (syntax->list #'(music-expr ...))]) (remove-from-id-ctxt e #'index))]]))
        #`(@ () #,(delete-expr s) #,(qq-art s (-- #,init-start result ...)))])))
