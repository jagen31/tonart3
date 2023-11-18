#lang racket

(require art
         "../rewriter/stdlib.rkt" "../rewriter/common-practice/lib.rkt" 
         "../realizer/electronic/lib.rkt" 
         "../realizer/electronic/linuxsampler/lib.rkt"
         "../realizer/visual/musicxml/lib.rkt"
         "../realizer/electronic/rsound/lib.rkt"
         "../rewriter/church/hymn.rkt"
  rsound (for-syntax syntax/parse))

#;(define-simple-rewriter do-it expand-do-it (loop 6 (i@ [0 6] (rhythm 2 2 2))))

#;(define-simple-rewriter the-music expand-the-music 

    (i@ [0 18] (instrument-map [strings . 069_Quintadena8Viola4]))

    (i@ [0 18] (do-it) (expand-do-it) (expand-loop) (seq (note a 0 3) (note b 0 3) (note c 0 4)) (apply-rhythm))
    )

#;(define linuxsampler-string
  (perform linuxsampler-performer 
    (i@ [0 18] (the-music))
    (expand-the-music)
    (i@ [0 18] (tempo 120) (instrument strings) (note->midi) (d/dt)) ; to midi events
    ))

#;(define musicxml-string 
  (perform musicxml-performer
    (i@ [0 18] (the-music))
    (expand-the-music) ; leave as notes
    ))

#;(displayln linuxsampler-string)
#;(displayln musicxml-string)

#;(define result 
  (perform music-rsound-performer 

    ;; the keys
    (ss@ (accomp) (-- 0 [3 (key d 0 major)] [3 (key c 0 minor)] [3 (key g 0 major)]))

    ;; repeat this pattern
    (musi@ [0 9 (accomp)] (loop 3 (-- 0 [1 (^ 2)] [1 (^ 5)] [1 (^ 1)])))

    (musi@ [0 9 (melody)] 
      (rhythm .5 .5 .5 .5 .5 .5 .5 .5 .5 .5 .5 .5 .5 .5 .5 .5 1)
      (seq (note a 0 5) (note g 1 5) (note a 0 5) (note b 0 5) (note a 0 5) (note f 1 5)
           (note e 0 5) (note d 0 5) (note e 0 5) (note c 0 5) (note g 0 5) (note f 1 5)
           (note g 0 5) (note d 0 5) (note b 0 4) (note d 0 5) (note g 0 4))
      (apply-rhythm))
      
    (measure@ 1 (ss@ (melody)) (instrument |Yamaha Grand Piano|) (octave 4))
    (measure@ 1 (ss@ (accomp)) (instrument |Yamaha Grand Piano|) (octave 4))

    (measure@ 1 (tempo 120) (expand-loop) (^->note) (note->midi))))

(define result (perform music-rsound-performer
    (ss@ (accomp)
      (measure@ 1
        (seq (voiced-chord c 1 [minor] (e 0 4) (c 1 4) (g 1 3))
             (chord a 0 [major]) (chord d 0 [major]) (chord g 1 [major 7]) (chord c 1 [minor]) 
             (chord g 1 [sus 4]) 
             (voiced-chord b 1 [diminished] _ _ (f 1 3))
             (voiced-chord c 1 [minor] _ (g 1 3) _)
             (voiced-chord c 1 [minor] _ (c 1 4) (g 1 3))
             (chord g 1 [sus 7]))
      (voice-lead 3)
      (i@ [0 24] (rhythm 8 2 2 1 1 1 1 1 3 4) (apply-rhythm))
      (voiced-chord->note-seq)
      (i@ [0 24]  (loop 1 (-- [1/3 (! 0)] [1/3 (! 1)] [1/3 (! 2)]) (apply-rhythm)))
      (expand-loop) (seq-ref)))
    (ss@ (bass)
      (measure@ 1
        (seq (note c 1 3) (note b 0 2) (note a 0 2) (note f 1 2) (note g 1 2) (note c 1 3) (note b 1 2))
        (rhythm 4 4 2 2 4 4 4)
        (apply-rhythm)))
    (music@ [(5 4) (melody)]
      (seq (note g 1 4) (note g 1 4) (note g 1 4) (note g 1 4) (note g 1 4))
      (rhythm 0.75 0.25 3 0.75 0.25)
      (apply-rhythm))
    (metric-interval->interval)
    (note->midi)
    (measure@ 1 (instrument |Yamaha Grand Piano|) (tempo 66))))

(set-output-device! 1)
(play result)
(rs-write result "moonlight.wav")

#;(define-simple-rewriter flammis-rhythm expand-flammis
    (-- [5 (rhythm 0.75 0.25 0.75 0.25 0.75 0.25 1 1)]))

#;(perform quote-performer
  (i@ [0 24]
    (music@ [(4 4) (basses)]
      (-- [2 (seq (note d 0 4) (note d 0 4) (note d 0 4) (note e 0 3))]
          [5 (seq (note g 1 3) (note e 0 3)
                  (note c 0 4) (note a 0 3) (note c 0 4) (note a 0 3) 
                  (note g 1 3) (note e 0 3))])
      (-- [2 (rhythm 0.75 0.25 0.75 0.25)] 
          [5 (flammis-rhythm)])))
  (i@ [0 24] (expand-flammis) (apply-rhythm)))
