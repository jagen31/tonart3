#lang racket

(require (except-in xml attribute) xml/path syntax/parse (for-syntax syntax/parse))

(define test 
   (syntax:read-xml (open-input-file "bells.musicxml")))

(define-syntax xml-path
  (syntax-parser
    [(_ xml tag:id tag-more ...)
     #'(map (λ (x) (xml-path x tag-more ...))
         (filter 
           (λ (expr) 
             (syntax-parse expr
               [(head:id _ _ (... ...)) (free-identifier=? #'head #'tag)]
               [_ #f]))
           (syntax-parse xml 
             [(_ _ body (... ...)) (syntax->list #'(body (... ...)))])))]
    [(_ xml) #'xml]))

(xml-path test part measure)