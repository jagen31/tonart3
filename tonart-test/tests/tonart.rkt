#lang racket

(require art (for-syntax ee-lib) 
  tonart/rewriter/common-practice/main 
  tonart/realizer/electronic/rsound/main
  tonart/realizer/electronic/linuxsampler/lib)

(set-output-device! 1)

(define sound (perform linuxsampler-performer
  (put 
    (seq ; open a sequence to do some voice leading
      (chords [f 1 M] [g 1 m] [c 1 M 7] [f 1 m] [b 0 m] [a 0 M] [d 0 M] [e 0 dim] [c 1 M 7] [f 1 m])
      (chord->voiced-chord 4)
      (put (seq (notes (f 1 2) (b 0 2) (c 1 3) (f 1 2) (b 0 2) (a 0 2) (d 0 3) (e 0 3) (c 1 3) (f 1 2))))
      (fill-voice 0) ; fill the 0th voice (bassline)
      (put (seq (notes (f 1 2) (a 0 3) (c 1 4) (f 1 4))))
      (fill-harmony 0) ; fill the first chord
      (voice-lead 4) ; fill in the rest of the voice leading
      ))

  (i@ [0 16] ; apply a rhythm to turn the seq into music.  write in the notes for the chords, using some bs.
    (rhythm 2 2 2 2 1 1 1 1 2 2)
    (&&& (apply-rhythm) (rhythm->holes)) ; do both of these things, using only one rhythm
    (voiced-chord->note-seq) 
    (! 0) (! 1) (! 2) (! 3) (fill-holes !) (seq-ref))

  (note->midi)
  (d/dt)
  
  (measure@ 1 
    (instrument-map [organ . 000/003_Montre_8_Prestant_4])
    (instrument organ)
    (tempo 120))))
  
#;(play sound)

(define file (open-output-file "tonart-lib/realizer/electronic/linuxsampler/.test/test.cpp" 
                               #:exists 'replace))
(displayln sound file)
(close-output-port file)