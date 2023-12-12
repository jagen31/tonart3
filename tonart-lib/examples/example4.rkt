#lang racket

(require art art/sequence/lib art/timeline/lib
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

    (i@ [0 18] (do-it) (expand-do-it) (expand-loop) (seq (ix-- (note a 0 3) (note b 0 3) (note c 0 4))) (apply-rhythm))
    )

#;(define linuxsampler-string
  (perform (linuxsampler-performer) 
    (i@ [0 18] (the-music))
    (expand-the-music)
    (i@ [0 18] (tempo 120) (instrument strings) (note->midi) (d/dt)) ; to midi events
    ))

#;(define musicxml-string 
  (perform (musicxml-performer)
    (i@ [0 18] (the-music))
    (expand-the-music) ; leave as notes
    ))

#;(displayln linuxsampler-string)
#;(displayln musicxml-string)

#;(define result 
  (perform (music-rsound-performer) 

    ;; the keys
    (ss@ (accomp) (-- 0 [3 (key d 0 major)] [3 (key c 0 minor)] [3 (key g 0 major)]))

    ;; repeat this pattern
    (musi@ [0 9 (accomp)] (loop 3 (-- 0 [1 (^ 2)] [1 (^ 5)] [1 (^ 1)])))

    (musi@ [0 9 (melody)] 
      (rhythm .5 .5 .5 .5 .5 .5 .5 .5 .5 .5 .5 .5 .5 .5 .5 .5 1)
      (seq (ix-- (note a 0 5) (note g 1 5) (note a 0 5) (note b 0 5) (note a 0 5) (note f 1 5)
           (note e 0 5) (note d 0 5) (note e 0 5) (note c 0 5) (note g 0 5) (note f 1 5)
           (note g 0 5) (note d 0 5) (note b 0 4) (note d 0 5) (note g 0 4)))
      (apply-rhythm))
      
    (measure@ 1 (ss@ (melody)) (instrument |Yamaha Grand Piano|) (octave 4))
    (measure@ 1 (ss@ (accomp)) (instrument |Yamaha Grand Piano|) (octave 4))

    (measure@ 1 (tempo 120) (expand-loop) (^->note) (note->midi))))

(define result
  (perform (linuxsampler-performer)
  (ss@ (accomp)
    (seq (chords (c 1 m #:v [(g 1 3) (c 1 4) (e 0 4)]) (a 0 M) (d 0 M) (g 1 M 7) (c 1 m) (g 1 sus 4) 
            (b 1 dim #:v [(f 1 3) _ _]) 
            (c 1 m #:v [(e 0 3) (g 1 3) _]) (c 1 m #:v [(g 1 3) (c 1 4) _])
            (g 1 sus 7) (c 1 m) (f 1 m) (e 0 M) (b 0 M 7 #:v [(a 0 3) _ _]) 
            (e 0 M) (e 0 m) (g 0 M 7)) ; [seq chord]
         (chord->voiced-chord 3)
         (voice-lead 3) ; [seq [voiced-chord 3]]
    )
    (rhythm 8 2 2 1 1 1 1 1 3 4 2 2 2 2 4 4 4)
    (apply-rhythm)

    (i@ [0 44] (loop 1 (i@ [0 1] (seq (ix-- (! 0) (! 1) (! 2))) (rhythm 1/3 1/3 1/3)))
               (expand-loop) (apply-rhythm))

    (voiced-chord->note-seq)
    (seq-ref)

    (instrument piano))

  (ss@ (bass)
    (seq (notes (c 1 3) (b 0 2) (a 0 2) (f 1 2) (g 1 2) (c 1 3) (b 1 2) (c 1 3) (f 1 2) (b 0 2) (b 0 2) 
                (e 0 3) (e 0 3) (d 0 3)))
    (rhythm 4 4 2 2 4 4 4 2 2 2 2 4 4 4)
    (apply-rhythm)
    (instrument pedal)
    (instrument piano)
    (instrument flute))

  (ss@ (melody)
    (mi@ [(5 4) (10 4)]
      (seq (notes (g 1 4) (g 1 4) (g 1 4) (g 1 4) (g 1 4) (g 1 4) (a 0 4) (g 1 4) (f 1 4) (b 0 4) (e 0 4)))
      (rhythm 0.75 0.25 3 0.75 0.25 2 2 2 1 1 1))

    (mi@ [(10 4)]
      (seq (notes (g 0 4) (g 0 4) (g 0 4) (g 0 4) (g 0 4)))
      (rhythm 0.75 0.25 3 0.75 0.25))

    (apply-rhythm)
    (instrument flute))

  (measure@ 1
    (note->midi)
    (instrument-map 
      [piano . 000/000_Montre_8]
      [pedal . 000/000_Montre_8]
      [flute . 000/003_Montre_8_Prestant_4])
    (tempo 66)
    (metric-interval->interval)
    (apply-tempo)
    (midi->full-midi)
    (d/dt))))

#|
(set-output-device! 1)
(play result)
(rs-write result "moonlight.wav")
|#

    (define file (open-output-file "tonart-lib/realizer/electronic/linuxsampler/.test/test.cpp" 
                                   #:exists 'replace))
    (displayln result file)
    (close-output-port file)

#;(define-simple-rewriter flammis-rhythm expand-flammis
    (-- [5 (rhythm 0.75 0.25 0.75 0.25 0.75 0.25 1 1)]))

#;(perform (quote-performer)
  (i@ [0 24]
    (music@ [(4 4) (basses)]
      (-- [2 (seq (ix-- (note d 0 4) (note d 0 4) (note d 0 4) (note e 0 3)))]
          [5 (seq (ix-- (note g 1 3) (note e 0 3)
                  (note c 0 4) (note a 0 3) (note c 0 4) (note a 0 3) 
                  (note g 1 3) (note e 0 3)))])
      (-- [2 (rhythm 0.75 0.25 0.75 0.25)] 
          [5 (flammis-rhythm)])))
  (i@ [0 24] (expand-flammis) (apply-rhythm)))
