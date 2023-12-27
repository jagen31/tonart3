#lang racket

(require art art/sequence art/timeline
         tonart/private/lib tonart/private/common-practice/lib 
         (for-syntax syntax/parse))
(provide (all-defined-out))

(define-art-object (st-flavian []))

(define-mapping-rewriter (st-flavian->rhythm [(: melodies st-flavian)])
  (λ (stx melody)
    (rewrite1
      (qq-art melody
        (rhythm 1 1 1 1 1 1 1 1 1 1 1 1 1 3 1 1 1 1 1 1 1 1 1 1 1 1 1 3)))))

(define-mapping-rewriter (st-flavian->^s [(: melodies st-flavian)])
  (λ (stx melody)
    (rewrite1
      (qq-art melody
        (@ ()
          (scale-degree-seq 1 1 0 1 3 2 2 1 1 4 3 1 2 3 3 3 4 5 3 1 2 3 3 2 1 1 0 1)
          (st-flavian)
          (st-flavian->rhythm)
          (apply-rhythm))))))

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
    (rewrite1
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