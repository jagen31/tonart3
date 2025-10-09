#lang racket

(require art tonart art/coordinate/index (for-syntax syntax/parse) art/sequence/ravel)


(define-art-object (char []))

(define-art doc
  (@ [(index 0 0)] (char "a")))

(define-art-rewriter overwrite
  (λ (stx)
    (syntax-parse stx
      [(_ ch:str)
       (qq-art stx (context (delete char) (char ch)))])))

(define-art-rewriter shift
  (λ (stx)
    (define ch (car (context-ref*/within (current-ctxt) (get-id-ctxt stx) #'char)))
    (define ix (expr-index ch))
    #`(context #,(delete-expr ch)
               #,(put-in-id-ctxt (remove-from-id-ctxt ch #'index) #`(index #,(car ix) #,(add1 (cadr ix)))))))

(define-art-rewriter insert
  (λ (stx)
    (define loc (expr-index stx))
    (syntax-parse stx
      [(_ ch:str)
       ;; FIXME jagen31 ohdear runtime
       #:with (sh ...)
       (sort
        (for/fold ([acc '()])
                  ([expr (current-ctxt)])
          (define loc2 (expr-index expr))
          (if (and loc2
                   (= (car loc) (car loc2))
                   (>= (cadr loc2) (cadr loc)))
              (cons #`(@ [(index #,@loc2)] (shift)) acc)
              acc))
        >
        #:key (λ (expr) (cadr (expr-index))))
         
       #`(context sh ... #,(qq-art stx (char ch)))])))

(define-art-object (cursor []))

(define-art-rewriter rewrite-at-cursor
  (λ (stx)
    (define curs (car (context-ref*/within (current-ctxt) (get-id-ctxt stx) #'cursor)))
    (define ctxt (get-id-ctxt curs))
    (syntax-parse stx
      [(_ expr ...)
       (qq-art stx (@ [#,@(get-id-ctxt curs)] expr ...))])))

(define-art-rewriter canon-from-cursor
  (λ (stx)
    (define cursors (context-ref*/within (current-ctxt) (get-id-ctxt stx) #'cursor))
    (define sorted (sort cursors < #:key (λ (curs) (expr-interval-start curs))))
    (define start (expr-interval-start stx))
    (define end (expr-interval-end stx))
    (println sorted)
    (println end)
    (define result
      (for/fold ([acc '()])
                ([c cursors])
        (define start* (expr-interval-start c))
        (cons #`(i@ #,start* #,@(filter (λ(x)
                                          (context-within? (get-id-ctxt x) (remove-from-ctxt (get-id-ctxt c) #'interval)
                                                           (lookup-ctxt)))
                                        (current-ctxt))) acc)))
    #`(context #,@result)))
