#lang racket

(require art (for-syntax ee-lib) 
  tonart/rewriter/common-practice/main 
  tonart/realizer/electronic/rsound/main
  tonart/realizer/electronic/linuxsampler/lib)

(set-output-device! 1)

(interpretation+ test
  [do-it
   (seq ; start a sequence to do some voice leading
    (chords [f 1 m] [g 1 m] [c 1 M 7] [f 1 m] [b 0 m] [a 0 M] [d 0 M] [e 0 dim] [c 1 M 7] [f 1 m]
            [g 0 M] [g 0 M 7] [c 1 M] [f 1 m] [g 1 M 7] [f 1 m] [c 1 M 7] [f 1 m] [g 1 dim 7] [c 1 M] [f 1 m])
    (chord->voiced-chord 5)

    (seq (notes (f 1 2) (b 0 2) (c 1 3) (f 1 2) (b 0 2) (a 0 2) (d 0 3) (e 0 3) (c 1 3) (f 1 2)
                (g 0 2) (b 0 2) (e 1 2) (f 1 2) (g 1 2) (a 0 2) (b 0 2) (a 0 2) (g 1 2) (c 1 3) (f 1 2)))
    (fill-voice 0) ; fill the 0th voice (bassline)

    (seq (notes (f 1 2) (c 1 3) (a 0 3) (c 1 4) (f 1 4)))
    (fill-harmony 0) ; fill the first chord

    (voice-lead 5) ; fill in the rest of the voice leading
    )

  (i@ [0 34] ; apply a rhythm to turn the seq into music.  write in the notes for the chords, using some bs.
    (rhythm 2 2 2 2 1 1 1 1 2 2 2 2 2 2 1 1 1 1 1 1 4)
    (&&& (apply-rhythm) (rhythm->holes)) ; do both of these things, using only one rhythm
    (voiced-chord->note-seq) 
    (! 0) (! 1) (! 2) (! 3) (! 4) (fill-holes !) (seq-ref))
  ])

(define sound (perform linuxsampler-performer 
  (measure@ 1 
    (instrument-map 
      [organ1 . |000/000_Montre_8|]
      [organ2 . |000/003_Montre_8_Prestant_4|]
      [organ3 . |000/003_Montre_8_Prestant_4|]
      [organ4 . |000/003_Montre_8_Prestant_4|])

    (-- [34 (do-it)] [34 (do-it)])
    (-- [16 (instrument organ1)] [18 (instrument organ2)] 
        [16 (instrument organ3)] [18 (instrument organ4)])
    (interpret test)

    (metric-interval->interval)
    (note->midi)
    (midi->full-midi)

    (d/dt)

    (tempo 120))))
  
#;(play sound)

(define file (open-output-file "tonart-lib/realizer/electronic/linuxsampler/.test/test.cpp" 
                               #:exists 'replace))
(displayln sound file)
(close-output-port file)