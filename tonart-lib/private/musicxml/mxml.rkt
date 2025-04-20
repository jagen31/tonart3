#lang at-exp racket

(require (for-meta -1 (except-in art number)) (except-in xml attribute) xml/path syntax/parse racket/string (for-syntax syntax/parse))

(provide (all-defined-out))

(define-syntax xml-path
  (syntax-parser
    [(_ xml tag:id tag-more ...)
     #'(foldr (λ (x acc) (define result (xml-path x tag-more ...)) (if (list? result) (append result acc) (cons result acc))) '()
         (filter 
           (λ (expr) 
             (syntax-parse expr
               [(head:id _ _ (... ...)) (eq? (syntax-e #'head) (syntax-e #'tag))]
               [_ #f]))
           (syntax-parse xml 
             [(_ _ body (... ...)) (syntax->list #'(body (... ...)))])))]
    [(_ xml) #'xml]))


(define-syntax xml-attr
  (syntax-parser
    [(_ xml tag:id)
     #'(syntax-parse xml 
         [(_ ([tag* value] (... ...)) _ (... ...))
          (define result
            (findf (λ (x) (syntax-parse x [(k v) (eq? (syntax-e #'k) (syntax-e #'tag))]))
                   (syntax->list #'([tag* value] (... ...)))))
          (cadr (syntax->list result))])]))

(define (xml-value xml)
  (syntax-parse xml
    [(_ _ body ...) (car (syntax->list #'(body ...)))]))

(define (xml-values xml)
  (syntax-parse xml
    [(_ _ body ...) (syntax->list #'(body ...))]))

(module+ test
  (define test 
     (syntax:read-xml (open-input-file "bells.musicxml")))

  (xml-path (cadr (xml-path (caddr (xml-path test part measure)) note)) dot))
