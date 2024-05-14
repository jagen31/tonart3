#lang racket

(require art art/sequence/ravel
         tonart/private/lib tonart/private/common-practice/lib 
         datenart
         (for-syntax syntax/parse))

(define-art-object (meter [parts]))

(define-mapping-rewriter (inline-music [(: m music)])
  (λ (stx m)
    (define/syntax-parse (_ expr ...) m)
    #'(context expr ...)))

(define-mapping-rewriter (inline-row [(: r row)])
  (λ (stx r)
    (define/syntax-parse (_ expr ...) r)
    (qq-art (remove-from-id-ctxt r #'table) (context expr ...))))

(define-art st-flavian-rhythm
  (rhythm 1 1 1 1 1 1 1 1 1 1 1 1 1 3 1 1 1 1 1 1 1 1 1 1 1 1 1 3))
(define-art st-flavian-^s
  (seq (^s 1 1 0 1 3 2 2 1 1 4 3 1 2 3 3 3 4 5 3 1 2 3 3 2 1 1 0 1)))

(define-art st-anne-rhythm
  (rhythm 1 1 1 1 1 1 1 1 1 1 1 1 1 3 1 1 1 1 1 1 1 1 1 1 1 1 1 3))
(define-art st-anne-^s
  (seq (^s 5 3 6 5 8 8 7 8 5 8 5 6 [4 1] 5 7 8 6 9 7 8 6 7 5 6 8 9 7 8)))

;; FIXME how to treat fermati? ??
(define-art doxology-rhythm
  (rhythm 1 1 1 1 1 2 2 3 1 1 1 1 1 2 2 3 1 1 1 1 1 2 2 3 1 1 1 1 1 2 2 4))
(define-art doxology-^s
  (seq (^s 1 1 0 -1 -2 1 2 3 3 3 3 2 1 4 3 2 1 2 3 2 1 -1 0 1 5 3 1 2 4 3 2 1)))

(define-art nettleton-a-rhythm (rhythm 1 1 2 2 1 1 2 2 1 1 2 2 1 1 4))
(define-art nettleton-a-^s (seq (^s 3 2 1 1 3 5 2 2 3 5 6 5 3 2 1)))
(define-art nettleton-b-rhythm (rhythm 1 1/2 1/2 2 2 1 1 1 1 2 1 0.5 0.5 2 2 1 1 4))
(define-art nettleton-b-^s (seq (^s 5 6 7 8 7 6 5 6 5 3 5 6 7 8 7 6 5 8)))
(define-art nettleton-rhythm
  (seq
   (ix-- (music nettleton-a-rhythm) (music nettleton-a-rhythm) (music nettleton-b-rhythm) (music nettleton-a-rhythm)))
  (rewrite-in-seq (rewrite-in-music (rhythm->holes))) (inline-music-seq) (unapply-rhythm hole) (delete seq))
(define-art nettleton-^s
  (ix-- nettleton-a-^s nettleton-a-^s nettleton-b-^s nettleton-a-^s)
  (coalesce-seq))

(define-art stuttgart-rhythm
  (rhythm 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2))
(define-art stuttgart-^s
  (seq (^s -2 -2 1 1 2 2 3 1 5 5 6 4 2 5 3 3 3 2 3 1 2 1 0 1 -1 -2 1 1 0 1)))

(define-art picardy-rhythm
  (rhythm 1 1 1 1 2 1 1 2 2 1 1 1 1 2 1 1 4 
          1 1 1 1 2 1 1 2 2 1 1 1 1 2 1 1 4 
          1 1 1 1 3 1 1 1 1 1 4 1 1 1 1 2 1 1 4))
(define-art picardy-^s
  (seq (^s 1 2 3 4 5 5 4 5 5 5 5 6 7 6 5 4 5 
           1 2 3 4 5 5 4 5 5 5 5 6 7 6 5 4 5 
           5 5 8 5 4 3 1 3 5 3 2 5 5 8 5 4 2 3 1)))

(define-art regent-square-rhythm
  (rhythm 1 1 1 1 1.5 0.5 1 1 1 1 1 1 1 1 2 1 1 1 1 1.5 0.5 1 1 1 1 1 0.5 0.5 1 1 2 1.5 0.5 1 1 1.5 0.5 1 1 1 1 1 1 1 1 1))
(define-art regent-square-^s
  (seq (^s 5 3 8 5 10 9 8 5 6 6 5 8 5 4 3 5 3 8 5 10 9 8 7 8 7 6 7 8 7 6 5 9 9 7 5 10 9 8 6 11 10 9 8 8 7 8)))

(define-art happy-birthday-rhythm
  (rhythm 0.75 0.25 1 1 1 2 0.75 0.25 1 1 1 2 0.75 0.25 1 1 1 1 2 0.75 0.25 1 1 1 2))
(define-art happy-birthday-^s
  (seq (^s 5 5 6 5 8 7 5 5 6 5 9 8 5 5 12 10 8 7 6 11 11 10 8 9 8)))

(define-art hymnal-table-header
  (@ [(table hymnal-table)]
    (column-names name rhythm ^s meter)

    (column-art-types symbol (music rhythm) (music ^) meter)
    (column-sql-types (varchar 20) (varchar) (varchar) (varchar))))

(define-art hymnal-table-sql
  (@ [(table hymnal-table)]
    (row (symbol st-flavian) (music st-flavian-rhythm) (music st-flavian-^s) (meter 8 6 8 6))
    (row (symbol st-anne) (music st-anne-rhythm) (music st-anne-^s) (meter 8 6 8 6))
    (row (symbol doxology) (music doxology-rhythm) (music doxology-^s) (meter 8 8 8 8))
    (row (symbol stuttgart) (music stuttgart-rhythm) (music stuttgart-^s) (meter 8 7 8 7))
    (row (symbol picardy) (music picardy-rhythm) (music picardy-^s) (meter 8 7 8 7 8 7))
    (row (symbol nettleton) (music nettleton-rhythm) (music nettleton-^s) (meter 8 7 8 7 D))
    (row (symbol regent-square) (music regent-square-rhythm) (music regent-square-^s) (meter 8 7 8 7 8 7))
    (row (symbol happy-birthday) (music happy-birthday-rhythm) (music happy-birthday-^s) (meter 6 6 7 6))))

#;(realize (postgres-realizer) 
  hymnal-table-header 
  (@ [(table hymnal-table)] (table-header->ddl)) 
  hymnal-table-sql)

(provide (all-defined-out) (for-syntax (all-defined-out)))