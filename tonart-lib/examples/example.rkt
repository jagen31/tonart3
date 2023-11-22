#lang racket

(require art
         "../rewriter/stdlib.rkt" "../rewriter/common-practice/lib.rkt"
         "../rewriter/church/hymn.rkt"
         "../realizer/electronic/lib.rkt"
         "../realizer/electronic/rsound/lib.rkt"
  rsound (for-syntax syntax/parse))

(define-simple-rewriter my-music my-music->objs 
  (@ () (! 0) (! 1) (! 2)))

(define test (perform music-rsound-performer 
  (i@ [0 4] (tone 440))
  (i@ [0 6] (loop 2 (my-music))) ;; -> my-music => my-music
  (pocket-rewrite
    (i@ [0 12] (instrument |Violin|))
    (i@ [0 10] (midi 61))
    (translate 2))
  (my-music->objs) ;; my-music -> ! => !

  (-- 0 
    [2 (seq (note a 0 3) (note c 0 4) (tone 200))] ; -> [seq note] => !, seq]
    [2 (seq (note a 0 4) (note c 0 5) (tone 300))] ; -> [seq note] => !, seq
    [2 (seq (note f 0 4) (note d 0 5) (tone 555))]) ; -> [seq note] => !, seq
  (seq-ref) ; !@[seq A] -> A => note, seq

  (i@ [0 32] (st-flavian))
  (st-flavian->^s)

  (i@ [0 100] (instrument |Yamaha Grand Piano|))
  (pocket-rewrite
    (i@ [0 24] (stuttgart))
    (stuttgart->notes)
    (note->midi))

  (pocket-rewrite
    (i@ [6 32] (thaxted-a))
    (thaxted-a->notes)
    (note->midi))

  (i@ [0 100] (put (tuning 12tet))) ;; -> tuning => !, seq, tuning
  (note->tone) ; note@tuning -> tone => tone, seq, tuning
  (measure@ 1 (tempo 120))
))

(set-output-device! 1)

#;(perform music-pstream-performer 
  (musi@ [2 32 (one)] (instrument Clarinet))
  (-- 2 [15 (loop 6 (musi@ [0 6 (one)] (rhythm 2 2 2)))] [15 (loop 6 (musi@ [0 6 (one)] (rhythm 1 1 1)))])
  (musi@ [2 32 (one)] (expand-loop))
  (musi@ [2 32 (one)] (seq (midi 61) (midi 62) (midi 63)))
  (musi@ [2 32 (one)] (apply-rhythm))
  )

(play test)
(rs-write test "test.wav")
