#lang racket

(require art art/sequence art/timeline
         tonart/private/lib tonart/private/common-practice/lib 
         (for-syntax syntax/parse))
(provide (all-defined-out))

(define-art-object (happy-birthday []))

(define-mapping-rewriter (happy-birthday->rhythm [(: melodies happy-birthday)])
  (λ (stx melody)
  #`(context
    #,@(rewrite
      (qq-art melody
        (rhythm 0.75 0.25 1 1 1 2 0.75 0.25 1 1 1 2 0.75 0.25 1 1 1 1 2 0.75 0.25 1 1 1 2))))))

(define-mapping-rewriter (happy-birthday->^s [(: melodies happy-birthday)])
  (λ (stx melody)
  #`(context
    #,@(rewrite
      (qq-art melody
        (seq (^s 5 5 6 5 8 7 5 5 6 5 9 8 5 5 12 10 8 7 6 11 11 10 8 9 8)))))))

(define-mapping-rewriter (happy-birthday->tune [(: melodies happy-birthday)])
  (λ (stx melody)
    #`(context
    #,@(rewrite
      (qq-art melody
        (@ ()
          (happy-birthday) (happy-birthday->rhythm)
          (happy-birthday) (happy-birthday->^s)
          (apply-rhythm)))))))

(define-art-object (st-flavian []))

(define-mapping-rewriter (st-flavian->rhythm [(: melodies st-flavian)])
  (λ (stx melody)
  #`(context
    #,@(rewrite
      (qq-art melody
        (rhythm 1 1 1 1 1 1 1 1 1 1 1 1 1 3 1 1 1 1 1 1 1 1 1 1 1 1 1 3))))))

(define-mapping-rewriter (st-flavian->tune [(: melodies st-flavian)])
  (λ (stx melody)
    #`(context
    #,@(rewrite
      (qq-art melody
        (@ ()
          (seq (^s 1 1 0 1 3 2 2 1 1 4 3 1 2 3 3 3 4 5 3 1 2 3 3 2 1 1 0 1))
          (st-flavian)
          (st-flavian->rhythm)
          (apply-rhythm)))))))

(define-art-object (stuttgart []))

(define-mapping-rewriter (stuttgart->notes [(: melody stuttgart)])
  (λ (stx melody)
    (qq-art melody
      (-- 0 
        [1 (note d 0 4)] [1 (note d 0 4)] [1 (note g 0 4)] [1 (note g 0 4)]
        [1 (note a 0 4)] [1 (note a 0 4)] [1 (note b 0 4)] [1 (note g 0 4)]
        [1 (note d 0 5)] [1 (note d 0 5)] [1 (note e 0 5)] [1 (note c 0 5)]
        [1 (note a 0 4)] [1 (note d 0 5)] [2 (note b 0 4)] 
        [1 (note b 0 4)] [1 (note b 0 4)] [1 (note a 0 4)] [1 (note b 0 4)]
        [1 (note g 0 4)] [1 (note a 0 4)] [1 (note g 0 4)] [1 (note f 1 4)]
        [1 (note g 0 4)] [1 (note e 0 4)] [1 (note d 0 4)] [1 (note g 0 4)]
        [1 (note g 0 4)] [1 (note f 1 4)] [2 (note g 0 4)]))))

(define-art-object (picardy []))

(define-mapping-rewriter (picardy->rhythm [(: melodies picardy)])
  (λ (stx melody)
     (qq-art melody 
       (rhythm 1 1 1 1 2 1 1 2 2 1 1 1 1 2 1 1 4 
               1 1 1 1 2 1 1 2 2 1 1 1 1 2 1 1 4 
               1 1 1 1 3 1 1 1 1 1 4 1 1 1 1 2 1 1 4))))

(define-mapping-rewriter (picardy->^s [(: melodies picardy)])
  (λ (stx melody)
     #`(context 
       #,@(rewrite 
            (qq-art melody 
              (^s 1 2 3 4 5 5 4 5 5 5 5 6 7 6 5 4 5 
                  1 2 3 4 5 5 4 5 5 5 5 6 7 6 5 4 5 
                  5 5 8 5 4 3 1 3 5 3 2 5 5 8 5 4 2 3 1))))))

(define-art-object (thaxted-a []))

(define-mapping-rewriter (thaxted-a->notes [(: melody thaxted-a)])
  (λ (stx melody)
    (qq-art melody
      (-- 0 [1/2 (note d 0 4)] [1/2 (note f 0 4)] 
            [3/2 (note g 0 4)] [1/2 (note b -1 4)] [3/4 (note a 0 4)] [1/4 (note f 0 4)] 
            [1/2 (note b -1 4)] [1/2 (note c 0 5)] [1 (note b -1 4)] [1 (note a 0 4)] 
            [1/2 (note g 0 4)] [1/2 (note a 0 4)] [1 (note g 0 4)] [1 (note f 0 4)] [2 (note d 0 4)] [1/2 (note d 0 4)] [1/2 (note f 0 4)] 
            [3/2 (note g 0 4)] [1/2 (note b -1 4)] [3/4 (note a 0 4)] [1/4 (note f 0 4)] 
            [1/2 (note b -1 4)] [1/2 (note c 0 5)] [1 (note d 0 5)] [1 (note d 0 5)] 
            [1/2 (note d 0 5)] [1/2 (note c 0 5)] [1 (note b -1 4)] [1 (note c 0 5)] [2 (note b -1 4)]))))

(define-art-object (hyfrydol []))

(define-simple-rewriter hyf-motif1 x-hyf-motif1 (rhythm 2 1 1 1 1))

(define-simple-rewriter hyf-phrase1 x-hyf-phrase1
  (-- 0 [6 (rhythm 2 1 1.5 0.5 1)] [6 (hyf-motif1)] [12 (rhythm 2 1 2 1 1 1 1 3)]))


(define-mapping-rewriter (hyfrydol->rhythm [(: melody hyfrydol)])
  (λ(stx melody)
    (rewrite
      (qq-art melody
        (context
          (-- 0 [48 (loop 24 (hyf-phrase1))]
                [18 (loop 6 (hyf-motif1))] [6 (rhythm 1 1 1 3)]
                [12 (rhythm 1 1 1 1 1 1 1 1 1 0.5 0.5 0.5 0.5 1)] [6 (hyf-motif1)] [6 (rhythm 1.5 0.5 1 1)])
          (i@ [0 96] (expand-loop) (x-hyf-motif1) (x-hyf-motif1) #;(coalesce rhythm)))))))

(define-mapping-rewriter (hyfrydol->notes [(: melody hyfrydol)])
  (λ(stx melody)
    (qq-art melody
      (pocket-rewrite 
        (i@ [0 96] 
          (hyfrydol)
          (hyfrydol->rhythm))
        (-- 0
          [48
            (loop 24 
              (seq 
                (ix-- (note f 0 4) (note g 0 4) (note f 0 4) (note g 0 4) (note a 0 4)
                (note b -1 4) (note a 0 4) (note g 0 4) (note f 0 4) (note g 0 4)
                (note c 0 5) (note b -1 4) (note a 0 4) (note a 0 4) 
                (note g 0 4) (note f 0 4) (note g 0 4) (note f 0 4))))]
          [24 
            (seq 
              (ix-- (note c 0 5) (note c 0 5) (note c 0 5) (note b -1 4) (note a 0 4)
              (note b -1 4) (note b -1 4) (note b -1 0 5) (note a 0 4) (note g 0 4)
              (note a 0 4) (note a 0 4) (note a 0 4) (note b -1 4) (note c 0 5)
              (note c 0 5) (note b -1 4) (note a 0 4) (note g 0 4)))]
          [24
            (seq
              (ix-- (note c 0 5) (note a 0 4) (note c 0 5) (note b -1 4) (note g 0 4) (note b -1 4)
              (note a 0 4) (note f 0 4) (note a 0 4) (note g 0 4) (note a 0 4) (note b -1 4) (note a 0 4) (note g 0 4)
              (note c 0 5) (note c 0 5) (note d 0 5) (note c 0 5) (note b -1 4) 
              (note a 0 4) (note b -1 4) (note g 0 4) (note f 0 4)))])
        (i@ [0 96] (expand-loop) (apply-rhythm))))))
